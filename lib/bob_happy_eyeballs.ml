let src = Logs.Src.create "bob.happy-eyeballs"

module Log = (val Logs.src_log src : Logs.LOG)

let msgf fmt = Format.kasprintf (fun msg -> `Msg msg) fmt

type t = {
  mutable waiters :
    ((Ipaddr.t * int) * Unix.file_descr, [ `Msg of string ]) result Fiber.Ivar.t
    Happy_eyeballs.Waiter_map.t;
  mutable cancel_connecting : unit Fiber.Ivar.t Happy_eyeballs.Waiter_map.t;
  mutable he : Happy_eyeballs.t;
  dns : Bob_dns.t;
  timer_interval : float;
  timer_condition : Fiber.Condition.t;
  timer_mutex : Fiber.Mutex.t;
}

let try_connect ip port () =
  let fd =
    let fam =
      match ip with Ipaddr.V4 _ -> Unix.PF_INET | Ipaddr.V6 _ -> Unix.PF_INET6
    in
    Unix.socket ~cloexec:true fam SOCK_STREAM 0
  in
  let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
  let open Fiber in
  Fiber.connect fd addr >>= function
  | Ok () -> Fiber.return (Ok fd)
  | Error err -> Fiber.close fd >>= fun () -> Fiber.return (Error (`Unix err))

let rec act t action =
  let open Fiber in
  (match action with
  | Happy_eyeballs.Resolve_a host -> (
      Bob_dns.getaddrinfo t.dns Dns.Rr_map.A host >>| function
      | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_a (host, res))
      | Error (`Msg msg) -> Ok (Happy_eyeballs.Resolved_a_failed (host, msg)))
  | Happy_eyeballs.Resolve_aaaa host -> (
      Bob_dns.getaddrinfo t.dns Dns.Rr_map.Aaaa host >>| function
      | Ok (_, res) -> Ok (Happy_eyeballs.Resolved_aaaa (host, res))
      | Error (`Msg msg) -> Ok (Happy_eyeballs.Resolved_aaaa_failed (host, msg))
      )
  | Happy_eyeballs.Connect (host, id, (ip, port)) -> (
      let cancelled, cancel =
        let ivar = Fiber.Ivar.create () in
        (Fiber.wait ivar, ivar)
      in
      t.cancel_connecting <-
        Happy_eyeballs.Waiter_map.add id cancel t.cancel_connecting;
      Fiber.pick (try_connect ip port) (fun () ->
          cancelled >>| fun () -> Error (`Msg "Cancelled"))
      >>= fun result ->
      t.cancel_connecting <-
        Happy_eyeballs.Waiter_map.remove id t.cancel_connecting;
      match result with
      | Ok fd -> (
          let waiters, result =
            Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
          in
          t.waiters <- waiters;
          match result with
          | Some waiter ->
              Fiber.Ivar.fill waiter (Ok ((ip, port), fd));
              Fiber.return
                (Ok (Happy_eyeballs.Connected (host, id, (ip, port))))
          | None -> Fiber.close fd >>= fun () -> Fiber.return (Error ()))
      | Error (`Unix _err) -> assert false
      | Error (`Msg msg) ->
          Fiber.return
            (Ok (Happy_eyeballs.Connection_failed (host, id, (ip, port), msg))))
  | Happy_eyeballs.Connect_cancelled (_host, id) ->
      (match Happy_eyeballs.Waiter_map.find_opt id t.cancel_connecting with
      | None -> ()
      | Some waiter -> Fiber.Ivar.fill waiter ());
      Fiber.return (Error ())
  | Happy_eyeballs.Connect_failed (_host, id, msg) -> (
      let waiters, result =
        Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
      in
      t.waiters <- waiters;
      match result with
      | Some waiter ->
          Fiber.Ivar.fill waiter (Error (msgf "Connection failed: %s" msg));
          Fiber.return (Error ())
      | None -> Fiber.return (Error ())))
  >>= function
  | Error () -> Fiber.return ()
  | Ok ev ->
      let he, actions =
        Happy_eyeballs.event t.he (Mtime_clock.elapsed_ns ()) ev
      in
      t.he <- he;
      Fiber.parallel_iter ~f:(act t) actions

let handle_timer_actions t actions =
  Fiber.async @@ fun () -> Fiber.parallel_iter ~f:(fun a -> act t a) actions

let rec timer t =
  let open Fiber in
  let rec loop () =
    let he, cont, actions =
      Happy_eyeballs.timer t.he (Mtime_clock.elapsed_ns ())
    in
    t.he <- he;
    handle_timer_actions t actions;
    match cont with
    | `Suspend -> timer t
    | `Act -> Fiber.sleep t.timer_interval >>= loop
  in
  Fiber.Condition.wait t.timer_condition t.timer_mutex >>= loop

let create ?(happy_eyeballs = Happy_eyeballs.create (Mtime_clock.elapsed_ns ()))
    ?(dns = Bob_dns.create ()) ?(timer_interval = Duration.of_ms 10) () =
  let waiters = Happy_eyeballs.Waiter_map.empty
  and cancel_connecting = Happy_eyeballs.Waiter_map.empty
  and timer_condition = Fiber.Condition.create ()
  and timer_mutex = Fiber.Mutex.create () in
  (* XXX(dinosaure): actually, we don't use [timer_mutex]. We probably should
     remove it but it's needed to be close to what is required to calculate the PACK file. *)
  let timer_interval = Duration.to_f timer_interval in
  let t =
    {
      waiters;
      cancel_connecting;
      he = happy_eyeballs;
      dns;
      timer_interval;
      timer_condition;
      timer_mutex;
    }
  in
  Fiber.async (fun () -> timer t);
  t

let handle_actions t actions =
  List.iter (fun a -> Fiber.async (fun () -> act t a)) actions

type error = [ `Msg of string ]

let open_error = function Ok _ as v -> v | Error #error as v -> v

let to_sockaddr ipaddr port =
  Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port)

let connect_host t host ports =
  let waiter, notify =
    let ivar = Fiber.Ivar.create () in
    (Fiber.wait ivar, ivar)
  in
  let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
  t.waiters <- waiters;
  let ts = Mtime_clock.elapsed_ns () in
  let he, actions = Happy_eyeballs.connect t.he ts ~id host ports in
  t.he <- he;
  Fiber.Condition.signal t.timer_condition;
  handle_actions t actions;
  let open Fiber in
  waiter >>| fun result ->
  Log.debug (fun m ->
      m "Connection %s to %a after %a"
        (match result with Ok _ -> "ok" | Error _ -> "failed")
        Domain_name.pp host Duration.pp
        (Int64.sub (Mtime_clock.elapsed_ns ()) ts));
  open_error
    (Result.map
       (fun ((ipaddr, port), socket) -> (to_sockaddr ipaddr port, socket))
       result)

let connect_ip t addresses =
  let waiter, notify =
    let ivar = Fiber.Ivar.create () in
    (Fiber.wait ivar, ivar)
  in
  let waiters, id = Happy_eyeballs.Waiter_map.register notify t.waiters in
  t.waiters <- waiters;
  let ts = Mtime_clock.elapsed_ns () in
  let he, actions = Happy_eyeballs.connect_ip t.he ts ~id addresses in
  t.he <- he;
  Fiber.Condition.signal t.timer_condition;
  handle_actions t actions;
  let open Fiber in
  waiter >>| fun result ->
  Log.debug (fun m ->
      m "Connection %s to %a after %a"
        (match result with Ok _ -> "ok" | Error _ -> "failed")
        Fmt.(list ~sep:(any ", ") (pair ~sep:(any ":") Ipaddr.pp int))
        addresses Duration.pp
        (Int64.sub (Mtime_clock.elapsed_ns ()) ts));
  open_error
    (Result.map
       (fun ((ipaddr, port), socket) -> (to_sockaddr ipaddr port, socket))
       result)

let connect t = function
  | `Inet (inet_addr, port) ->
      connect_ip t [ (Ipaddr_unix.of_inet_addr inet_addr, port) ]
  | `Domain (domain, port) -> connect_host t domain [ port ]
