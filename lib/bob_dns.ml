open Fiber
module IM = Map.Make (Int)

module Transport = struct
  (* Dns_client.S
       with type io_addr =
         [ `Plaintext of Ipaddr.t * int
         | `Tls of Tls.Config.client * Ipaddr.t * int ]
        and type +'a io = 'a Fiber.t
        and type stack = unit = struct *)
  type io_addr =
    [ `Plaintext of Ipaddr.t * int | `Tls of Tls.Config.client * Ipaddr.t * int ]

  type +'a io = 'a Fiber.t
  type stack = unit

  type nameservers =
    | Static of io_addr list
    | Resolv_conf of {
        mutable nameservers : io_addr list;
        mutable digest : Digest.t option;
      }

  type t = {
    nameservers : nameservers;
    timeout_ns : int64;
    mutable fd : [ `Plain of Unix.file_descr ] option;
    mutable connected_condition : Fiber.Condition.t option;
    mutable requests :
      (Cstruct.t * (Cstruct.t, [ `Msg of string ]) result Fiber.t) IM.t;
    mutable he : Happy_eyeballs.t;
    mutable waiters :
      ((Ipaddr.t * int) * Unix.file_descr, [ `Msg of string ]) result
      Fiber.Ivar.t
      Happy_eyeballs.Waiter_map.t;
    timer_mutex : Fiber.Mutex.t;
    timer_condition : Fiber.Condition.t;
  }

  type context = t

  let nameserver_ips = function
    | Static nameservers -> nameservers
    | Resolv_conf { nameservers; _ } -> nameservers

  let handle_action' t action =
    match action with
    | Happy_eyeballs.Connect (host, id, (ip, port)) -> (
        let { Unix.p_proto; _ } = Unix.getprotobyname "tcp" in
        let fam =
          match ip with
          | Ipaddr.V4 _ -> Unix.PF_INET
          | Ipaddr.V6 _ -> Unix.PF_INET6
        in
        let socket = Unix.socket fam Unix.SOCK_STREAM p_proto in
        let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
        Fiber.connect socket addr >>= function
        | Error _errno ->
            Fiber.return
              (Some (Happy_eyeballs.Connection_failed (host, id, (ip, port))))
        | Ok () -> (
            let waiters, r =
              Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
            in
            t.waiters <- waiters;
            match r with
            | Some waiter ->
                Fiber.Ivar.fill waiter (Ok ((ip, port), socket));
                Fiber.return
                  (Some (Happy_eyeballs.Connected (host, id, (ip, port))))
            | None ->
                Fiber.close socket >>= fun () ->
                Fiber.return
                  (Some (Happy_eyeballs.Connected (host, id, (ip, port))))))
    | Happy_eyeballs.Connect_failed (_host, id) ->
        let waiters, r =
          Happy_eyeballs.Waiter_map.find_and_remove id t.waiters
        in
        t.waiters <- waiters;
        Stdlib.Option.iter
          (fun waiter ->
            Fiber.Ivar.fill waiter (Error (`Msg "Connection failed")))
          r;
        Fiber.return None
    | _action -> Fiber.return None

  let rec handle_action t action =
    handle_action' t action >>= function
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
        waiters = Happy_eyeballs.Waiter_map.empty;
        timer_mutex = Fiber.Mutex.create ();
        timer_condition = Fiber.Condition.create ();
      }
    in
    Fiber.async (fun () -> he_timer t);
    t
end
