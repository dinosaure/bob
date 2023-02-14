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
     and type stack = unit = struct
  type io_addr =
    [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

  type +'a io = 'a Fiber.t
  type stack = unit
  type nameservers = Static of io_addr list

  type t = {
    nameservers : nameservers;
    timeout_ns : int64;
    mutable fd : [ `Plain of Unix.file_descr | `Tls of Bob_tls.t ] option;
    mutable connected_condition : (Fiber.Condition.t * Fiber.Mutex.t) option;
    mutable requests :
      (Cstruct.t * (Cstruct.t, [ `Msg of string ]) result Fiber.Ivar.t) IM.t;
    mutable he : Happy_eyeballs.t;
    mutable cancel_connecting : unit Fiber.Ivar.t Happy_eyeballs.Waiter_map.t;
    mutable waiters :
      ((Ipaddr.t * int) * Unix.file_descr, [ `Msg of string ]) result
      Fiber.Ivar.t
      Happy_eyeballs.Waiter_map.t;
    timer_mutex : Fiber.Mutex.t;
    timer_condition : Fiber.Condition.t;
  }

  type context = t

  let nameserver_ips = function Static nameservers -> nameservers

  let handle_one_action t = function
    | Happy_eyeballs.Connect (host, id, (ip, port)) -> (
        let cancel = Fiber.Ivar.create () in
        t.cancel_connecting <-
          Happy_eyeballs.Waiter_map.add id cancel t.cancel_connecting;
        let { Unix.p_proto; _ } = Unix.getprotobyname "tcp" in
        let fam =
          match ip with
          | Ipaddr.V4 _ -> Unix.PF_INET
          | Ipaddr.V6 _ -> Unix.PF_INET6
        in
        let socket = Unix.socket fam Unix.SOCK_STREAM p_proto in
        let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
        Fiber.pick
          (fun () ->
            Fiber.connect socket addr >>= function
            | Error errno ->
                Fiber.return
                  (Error
                     (msgf "Error %s connecting to nameserver %a:%d"
                        (Unix.error_message errno) Ipaddr.pp ip port))
            | Ok _ as v -> Fiber.return v)
          (fun () -> Fiber.wait cancel >>| fun () -> Error (msgf "Cancelled"))
        >>= fun result ->
        t.cancel_connecting <-
          Happy_eyeballs.Waiter_map.remove id t.cancel_connecting;
        match result with
        | Error (`Msg err) ->
            Fiber.close socket >>| fun () ->
            Some (Happy_eyeballs.Connection_failed (host, id, (ip, port), err))
        | Ok () ->
            let waiters, v =
              Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
            in
            t.waiters <- waiters;
            (match v with
            | Some waiter ->
                Fiber.Ivar.fill waiter (Ok ((ip, port), socket));
                Fiber.return ()
            | None -> Fiber.close socket)
            >>| fun () -> Some (Happy_eyeballs.Connected (host, id, (ip, port)))
        )
    | Connect_failed (host, id, reason) ->
        let waiters, v =
          Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
        in
        t.waiters <- waiters;
        (match v with
        | Some waiter ->
            let err =
              msgf "Connection to %a failed: %s" Domain_name.pp host reason
            in
            Fiber.Ivar.fill waiter (Error err)
        | None -> ());
        Fiber.return None
    | Connect_cancelled (host, id) -> (
        match Happy_eyeballs.Waiter_map.find_opt id t.cancel_connecting with
        | None ->
            Log.warn (fun m ->
                m
                  "Happy-eyeballs tries to cancel the connection to %a but the \
                   process does not exist."
                  Domain_name.pp host);
            Fiber.return None
        | Some cancel ->
            Fiber.Ivar.fill cancel ();
            Fiber.return None)
    | (Resolve_a _ | Resolve_aaaa _) as a ->
        Log.warn (fun m -> m "Ignoring action %a" Happy_eyeballs.pp_action a);
        Fiber.return None

  let rec handle_action t action =
    handle_one_action t action >>= function
    | None -> Fiber.return ()
    | Some event ->
        let he, actions =
          Happy_eyeballs.event t.he (Mtime_clock.elapsed_ns ()) event
        in
        t.he <- he;
        Fiber.parallel_iter ~f:(handle_action t) actions

  let handle_timer_actions t actions =
    Fiber.async (fun () ->
        Fiber.parallel_iter ~f:(fun a -> handle_action t a) actions)

  let he_timer_interval = Duration.of_ms 500

  let rec he_timer t =
    let rec loop () =
      let he, cont, actions =
        Happy_eyeballs.timer t.he (Mtime_clock.elapsed_ns ())
      in
      t.he <- he;
      handle_timer_actions t actions;
      match cont with
      | `Suspend -> he_timer t
      | `Act -> Fiber.sleep (Duration.to_f he_timer_interval) >>= loop
    in
    Fiber.Condition.wait t.timer_condition t.timer_mutex >>= loop

  let create ?nameservers ~timeout:timeout_ns () =
    let nameservers =
      match nameservers with
      | Some (`Udp, _) -> invalid_arg "UDP is not supported"
      | Some (`Tcp, ns) -> Static ns
      | None -> assert false
    in
    let t =
      {
        nameservers;
        timeout_ns;
        fd = None;
        connected_condition = None;
        requests = IM.empty;
        he = Happy_eyeballs.create (Mtime_clock.elapsed_ns ());
        cancel_connecting = Happy_eyeballs.Waiter_map.empty;
        waiters = Happy_eyeballs.Waiter_map.empty;
        timer_mutex = Fiber.Mutex.create ();
        timer_condition = Fiber.Condition.create ();
      }
    in
    Fiber.async (fun () -> he_timer t);
    t

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
        let { Cstruct.buffer; off; len } = tx in
        Fiber.write fd buffer ~off ~len >>= function
        | Ok len' when len = len' -> Fiber.return (Ok ())
        | Ok len -> Fiber.return (Error (msgf "Partially send %d byte(s)" len))
        | Error err -> Fiber.return (Error (msgf "%a" pp_error err)))

  let send_recv (t : context) tx =
    if Cstruct.length tx > 4 then (
      match t.fd with
      | None ->
          Fiber.return
            (Error (msgf "No connection to the nameserver established"))
      | Some fd ->
          let id = Cstruct.BE.get_uint16 tx 2 in
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

  let rec read_loop ?(linger = Cstruct.empty) (t : t) fd =
    (match fd with
    | `Plain fd -> (
        Fiber.read fd >>= function
        | Ok `End | Error _ -> Fiber.return (0, Cstruct.empty)
        | Ok (`Data data) ->
            let data = Cstruct.of_bigarray data in
            Fiber.return (Cstruct.length data, data))
    | `Tls fd -> (
        let buf = Cstruct.create 2048 in
        Bob_tls.read fd buf >>= function
        | Ok len -> Fiber.return (len, Cstruct.sub buf 0 len)
        | Error err ->
            Log.err (fun m -> m "TLS read failed: %a" Bob_tls.pp_error err);
            Fiber.return (0, Cstruct.empty)))
    >>= function
    | 0, _ ->
        (match fd with
        | `Plain fd -> Fiber.close fd
        | `Tls fd -> Bob_tls.close fd)
        >>= fun () ->
        t.fd <- None;
        Fiber.return ()
    | _, cs ->
        let rec handle_data data =
          let cs_len = Cstruct.length data in
          if cs_len > 2 then
            let len = Cstruct.BE.get_uint16 data 0 in
            if cs_len - 2 >= len then (
              let packet, rest =
                if cs_len - 2 = len then (data, Cstruct.empty)
                else Cstruct.split data (len + 2)
              in
              let id = Cstruct.BE.get_uint16 packet 2 in
              (match IM.find_opt id t.requests with
              | None ->
                  Log.warn (fun m -> m "Received unsolicited data, ignoring")
              | Some (_, ivar) -> Ivar.fill ivar (Ok packet));
              handle_data rest)
            else read_loop ~linger:data t fd
          else read_loop ~linger:data t fd
        in
        handle_data
          (if Cstruct.length linger = 0 then cs else Cstruct.append linger cs)

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
    let ivar = Fiber.Ivar.create () in
    let waiters, id = Happy_eyeballs.Waiter_map.register ivar t.waiters in
    t.waiters <- waiters;
    let ns = to_pairs nameservers in
    let he, actions =
      Happy_eyeballs.connect_ip t.he (Mtime_clock.elapsed_ns ()) ~id ns
    in
    t.he <- he;
    Fiber.Condition.signal t.timer_condition;
    Fiber.async (fun () -> Fiber.parallel_iter ~f:(handle_action t) actions);
    Fiber.wait ivar >>= function
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

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
