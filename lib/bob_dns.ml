open Stdbob
open Fiber
module IM = Map.Make (Int)

let src = Logs.Src.create "bob.dns"

module Log = (val Logs.src_log src : Logs.LOG)

module Transport :
  Dns_client.S
    with type io_addr =
      [ `Plaintext of Ipaddr.t * int
      | `Tls of Tls.Config.client * Ipaddr.t * int ]
     and type +'a io = 'a Fiber.t
     and type stack = Bob_happy_eyeballs.t = struct
  type io_addr =
    [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

  type +'a io = 'a Fiber.t
  type stack = Bob_happy_eyeballs.t
  type nameservers = Static of io_addr list

  type t = {
    nameservers : nameservers;
    timeout_ns : int64;
    mutable fd : [ `Plain of Unix.file_descr | `Tls of Bob_tls.t ] option;
    mutable connected_condition : (Fiber.Condition.t * Fiber.Mutex.t) option;
    mutable requests :
      (string * (string, [ `Msg of string ]) result Fiber.Ivar.t) IM.t;
    he : Bob_happy_eyeballs.t;
  }

  type context = t

  let nameserver_ips = function Static nameservers -> nameservers

  let create ?nameservers ~timeout:timeout_ns he =
    let nameservers =
      match nameservers with
      | Some (`Udp, _) -> invalid_arg "UDP is not supported"
      | Some (`Tcp, ns) -> Static ns
      | None -> assert false
    in
    {
      nameservers;
      timeout_ns;
      fd = None;
      connected_condition = None;
      requests = IM.empty;
      he;
    }

  let nameservers { nameservers; _ } = (`Tcp, nameserver_ips nameservers)
  let rng = Mirage_crypto_rng.generate ?g:None

  let with_timeout timeout f =
    let timeout () =
      Fiber.sleep (Duration.to_f timeout) >>| fun () ->
      Error (`Msg "DNS request timeout")
    in
    Fiber.npick [ f; timeout ]

  let close _ = Fiber.return ()

  let pp_error ppf = function
    | #Bob_tls.error as err -> Bob_tls.pp_error ppf err
    | `Msg err -> Fmt.string ppf err

  let send_query fd tx =
    match fd with
    | `Tls fd -> (
        Bob_tls.write fd tx >>= function
        | Ok _ as v -> Fiber.return v
        | Error err -> Fiber.return (Error (msgf "%a" pp_error err)))
    | `Plain fd -> (
        Fiber.write fd tx ~off:0 ~len:(String.length tx) >>= function
        | Ok len' when String.length tx = len' -> Fiber.return (Ok ())
        | Ok len -> Fiber.return (Error (msgf "Partially send %d byte(s)" len))
        | Error err -> Fiber.return (Error (msgf "%a" pp_error err)))

  let send_recv (t : context) tx =
    if String.length tx > 4 then (
      match t.fd with
      | None ->
          Fiber.return
            (Error (msgf "No connection to the nameserver established"))
      | Some fd ->
          let id = String.get_uint16_be tx 2 in
          ( with_timeout t.timeout_ns @@ fun () ->
            send_query fd tx >>? fun () ->
            let ivar = Fiber.Ivar.create () in
            t.requests <- IM.add id (tx, ivar) t.requests;
            Fiber.wait ivar >>| function
            | Ok v -> Ok v
            | Error (`Msg err) -> Error (`Msg err) )
          >>| fun res ->
          t.requests <- IM.remove id t.requests;
          res)
    else Fiber.return (Error (msgf "Invalid DNS packet (data length <= 4)"))

  let split str off =
    let len = String.length str in
    (String.sub str 0 off, String.sub str off (len - off))

  let rec read_loop ?(linger = "") (t : t) fd =
    (match fd with
    | `Plain fd -> (
        Fiber.read fd >>= function
        | Ok `End | Error _ -> Fiber.return (0, "")
        | Ok (`Data data) -> Fiber.return (String.length data, data))
    | `Tls fd -> (
        let buf = Bytes.create 2048 in
        Bob_tls.read fd buf >>= function
        | Ok len -> Fiber.return (len, Bytes.sub_string buf 0 len)
        | Error err ->
            Log.err (fun m -> m "TLS read failed: %a" Bob_tls.pp_error err);
            Fiber.return (0, "")))
    >>= function
    | 0, _ ->
        (match fd with
        | `Plain fd -> Fiber.close fd
        | `Tls fd -> Bob_tls.close fd)
        >>= fun () ->
        t.fd <- None;
        Fiber.return ()
    | _, str ->
        let rec handle_data data =
          let str_len = String.length data in
          if str_len > 2 then
            let len = String.get_uint16_be data 0 in
            if str_len - 2 >= len then (
              let packet, rest =
                if str_len - 2 = len then (data, "") else split data (len + 2)
              in
              let id = String.get_uint16_be packet 2 in
              (match IM.find_opt id t.requests with
              | None ->
                  Log.warn (fun m -> m "Received unsolicited data, ignoring")
              | Some (_, ivar) -> Ivar.fill ivar (Ok packet));
              handle_data rest)
            else read_loop ~linger:data t fd
          else read_loop ~linger:data t fd
        in
        handle_data (if String.length linger = 0 then str else linger ^ str)

  let req_all fd t =
    IM.fold
      (fun _id (data, _) r ->
        r >>= function
        | Error _ as err -> Fiber.return err
        | Ok () -> send_query fd data)
      t.requests (Fiber.return (Ok ()))

  let to_pairs nameservers =
    List.map
      (function `Plaintext (ip, port) | `Tls (_, ip, port) -> (ip, port))
      nameservers

  let find_ns ns (addr, port) =
    List.find
      (function
        | `Plaintext (ip, p) | `Tls (_, ip, p) ->
            Ipaddr.compare ip addr = 0 && p = port)
      ns

  let rec connect_to_ns_list (t : t) connected_condition nameservers :
      (unit, [ `Msg of string ]) result Fiber.t =
    let ns = to_pairs nameservers in
    Bob_happy_eyeballs.connect_ip t.he ns >>= function
    | Error (`Msg msg) ->
        Fiber.Condition.broadcast connected_condition;
        t.connected_condition <- None;
        Fiber.return
          (Error
             (msgf "Error %s connecting to resolver %a" msg
                Fmt.(Dump.list (pair ~sep:(any ":") Ipaddr.pp int))
                (to_pairs (nameserver_ips t.nameservers))))
    | Ok (addr, socket) -> (
        let continue socket =
          t.fd <- Some socket;
          Fiber.async (fun () ->
              read_loop t socket >>= fun () ->
              if IM.is_empty t.requests then Fiber.return ()
              else
                connect_via_tcp_to_ns t >>| function
                | Ok () -> ()
                | Error err ->
                    Log.err (fun m ->
                        m "Error while connecting to resolver: %a" pp_error err));
          Fiber.Condition.broadcast connected_condition;
          t.connected_condition <- None;
          req_all socket t
        in
        let addr =
          match addr with
          | Unix.ADDR_INET (inet_addr, port) ->
              (Ipaddr_unix.of_inet_addr inet_addr, port)
          | Unix.ADDR_UNIX name -> Fmt.failwith "Got an UNIX address: %S" name
        in
        let config = find_ns (nameserver_ips t.nameservers) addr in
        match config with
        | `Plaintext _ -> continue (`Plain socket)
        | `Tls (tls_cfg, _, _) -> (
            Bob_tls.client_of_file_descr tls_cfg socket >>= function
            | Ok fd -> continue (`Tls fd)
            | Error err -> (
                Log.warn (fun m ->
                    m "TLS handshake with %a:%d failed: %a" Ipaddr.pp (fst addr)
                      (snd addr) Bob_tls.pp_error err);
                match
                  List.filter
                    (function
                      | `Tls (_, ip, port) ->
                          not
                            (Ipaddr.compare ip (fst addr) = 0 && port = snd addr)
                      | _ -> true)
                    nameservers
                with
                | [] ->
                    Fiber.Condition.broadcast connected_condition;
                    t.connected_condition <- None;
                    Fiber.return
                      (Error (`Msg "No further nameservers configured"))
                | ns' -> connect_to_ns_list t connected_condition ns')))

  and connect_via_tcp_to_ns (t : t) : (unit, [ `Msg of string ]) result Fiber.t
      =
    match (t.fd, t.connected_condition) with
    | Some _, _ -> Fiber.return (Ok ())
    | None, Some (condition, mutex) ->
        Fiber.Condition.wait condition mutex >>= fun () ->
        connect_via_tcp_to_ns t
    | None, None ->
        let mutex = Fiber.Mutex.create () in
        let connected_condition = (Fiber.Condition.create (), mutex) in
        t.connected_condition <- Some connected_condition;
        connect_to_ns_list t (fst connected_condition)
          (nameserver_ips t.nameservers)

  let connect t =
    connect_via_tcp_to_ns t >>| function
    | Ok () -> Ok (`Tcp, t)
    | Error (`Msg err) -> Error (`Msg err)

  let bind = Fiber.bind
  let lift = Fiber.return
  let clock = Mtime_clock.elapsed_ns
end

include Dns_client.Make (Transport)

let () = Mirage_crypto_rng_unix.use_default ()
