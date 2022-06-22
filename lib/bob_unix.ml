let src = Logs.Src.create "bob.unix"
module Log = (val Logs.src_log src : Logs.LOG)

open Fiber

let rec full_write fd str ~off ~len =
  Fiber.write fd str ~off ~len >>= function
  | Error _ as err -> Fiber.return err
  | Ok len' ->
    if len - len' > 0 then full_write fd str ~off:(off + len') ~len:(len - len')
    else Fiber.return (Ok ())

let full_read fd =
  let buf = Buffer.create 0x100 in
  let rec go () = Fiber.read fd >>= function
    | `Data str -> Buffer.add_string buf str ; go ()
    | `End -> Fiber.return (Buffer.contents buf) in
  go () >>= function
  | "" -> Fiber.return `End
  | str -> Fiber.return (`Data (str, 0, String.length str))

type error =
  [ `Connection_closed_by_relay
  | `Write of Unix.error
  | Bob.Protocol.error ]

let pp_error ppf = function
  | `Connection_closed_by_relay -> Fmt.string ppf "Connection closed by relay"
  | `Write err -> Fmt.pf ppf "write(): %s" (Unix.error_message err)
  | #Bob.Protocol.error as err -> Bob.Protocol.pp_error ppf err

let map_read = function
  | `Data str -> `Read (`Data (str, 0, String.length str))
  | `End -> `Read `End

type outcome =
  [ `Error of error
  | `Done
  | `Write of string
  | `Continue ]

type income =
  [ `Read of [ `Data of (string * int * int) | `End ]
  | `Error of error ]

let pp_data ppf = function
  | `End -> Fmt.pf ppf "<end>"
  | `Data (str, off, len) ->
    Fmt.pf ppf "@[<hov>%a@]" (Hxd_string.pp Hxd.default)
      (String.sub str off len)

let run ~choose ~agreement ~receive ~send socket t =
  let handshake_is_done : [ `Done ] Fiber.Ivar.t = Fiber.Ivar.create () in
  let errored : [ `Error of error ] Fiber.Ivar.t = Fiber.Ivar.create () in
  let rec read () =
    Fiber.npick
      [ begin fun () -> Fiber.read socket >>| map_read end
      ; begin fun () -> Fiber.wait errored >>| fun v ->
                        (v :> income) end ]
    >>= function
    | `Error err ->
      Log.err (fun m -> m "Got a global error: %a" pp_error err) ;
      Fiber.return (Error err)
    | `Read data ->
      Log.debug (fun m -> m "recv <- %a" pp_data data) ;
      match receive t data with
      | `Continue | `Read -> Fiber.pause () >>= read
      | `Agreement identity -> choose identity >>= fun v ->
        agreement t v ; Fiber.pause () >>= read
      | `Done shared_keys ->
        Fiber.Ivar.fill handshake_is_done `Done ;
        Fiber.return (Ok shared_keys)
      | `Close ->
        Log.err (fun m -> m "The relay closed the connection.") ;
        Fiber.Ivar.fill errored (`Error `Connection_closed_by_relay) ;
        Fiber.return (Error `Connection_closed_by_relay)
      | `Error (#Bob.Protocol.error as err) ->
        Log.err (fun m -> m "Got a recv error: %a" Bob.Protocol.pp_error err) ;
        Fiber.Ivar.fill errored (`Error (err :> error)) ;
        Fiber.return (Error err) in
  let rec write () =
    let send () = Fiber.return (send t) in
    Fiber.npick
      [ begin fun () -> Fiber.wait handshake_is_done >>| fun v ->
                        (v :> outcome) end
      ; begin fun () -> Fiber.wait errored >>| fun v ->
                        (v :> outcome) end
      ; begin fun () -> send () >>| fun v ->
                        (v :> outcome) end ] >>= function
    | `Done -> Fiber.return (Ok ())
    | `Continue -> Fiber.pause () >>= write
    | `Error err ->
      Log.err (fun m -> m "Got an error: %a" pp_error err) ;
      if Fiber.Ivar.is_empty errored
      then Fiber.Ivar.fill errored (`Error (err :> error)) ;
      Fiber.return (Error err)
    | `Write str ->
      Log.debug (fun m -> m "send -> @[<hov>%a@]"
        (Hxd_string.pp Hxd.default) str) ;
      full_write socket str ~off:0 ~len:(String.length str) >>= function
      | Ok () -> write ()
      | Error err ->
        Log.err (fun m -> m "Got a write error: %s" (Unix.error_message err)) ;
        if Fiber.Ivar.is_empty errored
        then Fiber.Ivar.fill errored (`Error (`Write err)) ;
        Fiber.return (Error (`Write err)) in
  Fiber.fork_and_join write read >>= function
  | Ok (), Ok shared_keys -> Fiber.return (Ok shared_keys)
  | Error err, _ -> Fiber.return (Error err)
  | _, Error err -> Fiber.return (Error err)

let server socket ~g ~secret =
  let t = Bob.Server.hello ~g ~secret in
  let choose _ = assert false in
  let agreement _ = assert false in
  run ~choose ~agreement
    ~receive:Bob.Server.receive ~send:Bob.Server.send socket t >>= fun res ->
  Fiber.close socket >>= fun () -> Fiber.return res

let client socket ~choose ~g ~password =
  let identity = Unix.gethostname () in
  let t = Bob.Client.make ~g ~password ~identity in
  run ~choose ~agreement:Bob.Client.agreement
    ~receive:Bob.Client.receive ~send:Bob.Client.send socket t >>= fun res ->
  (* TODO(dinosaure): when we refuse, we should keep alive the socket. *)
  Fiber.close socket >>= fun () -> Fiber.return res

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
    Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let serve_when_ready ?stop ~handler socket =
  let stop = match stop with
    | Some stop -> stop
    | None -> Fiber.Ivar.create () in
  let rec loop () =
    Fiber.pick
      begin fun () -> Fiber.wait stop >>| fun () -> `Stop end
      begin fun () -> Fiber.accept socket >>| fun v -> `Accept v end
    >>= function
    | `Stop -> Fiber.return ()
    | `Accept (fd, sockaddr) ->
      Log.debug (fun m -> m "Got a new connection from %a."
        pp_sockaddr sockaddr) ;
      let _ = Fiber.async begin fun () -> handler fd sockaddr end in
      Fiber.pause () >>= loop in
  loop ()

let relay ?(timeout= 5.) socket ~stop =
  let t = Bob.Relay.make () in
  let fds = Hashtbl.create 0x100 in
  let rec write () =
    let send_to () = Fiber.return (Bob.Relay.send_to t) in
    Fiber.npick
      [ begin fun () -> Fiber.wait stop >>| fun () -> `Stop end
      ; send_to ]
    >>= function
    | `Stop     -> Fiber.return ()
    | `Continue -> Fiber.pause () >>= write
    | `Close identity ->
      ( match Hashtbl.find_opt fds identity with
      | Some  _ -> Hashtbl.remove fds identity ; Fiber.return ()
      | None -> Fiber.return () ) >>= write
    | `Write (identity, str) ->
      ( match Hashtbl.find_opt fds identity with
      | None -> Fiber.return ()
      | Some (fd, _) ->
        Log.debug (fun m -> m "to   [%20s] <- @[<hov>%a@]"
          identity (Hxd_string.pp Hxd.default) str) ;
        full_write fd str ~off:0 ~len:(String.length str) >>= function
        | Ok v -> Fiber.return v
        | Error _err ->
          Hashtbl.remove fds identity ;
          Fiber.close fd ) >>= write in
  let rec read () =
    Fiber.npick [ begin fun () -> Fiber.wait stop >>| fun () -> `Stop end
                ; begin fun () -> Fiber.return `Continue end ] >>= function
    | `Stop -> Fiber.return ()
    | `Continue ->
      Hashtbl.filter_map_inplace begin fun identity (fd, ivar) ->
      match Fiber.Ivar.get ivar with
      | None -> Some (fd, ivar)
      | Some `Delete -> None
      | Some `Continue ->
        let ivar = Fiber.detach begin fun () ->
          Fiber.read fd >>| map_read >>= fun (`Read data) ->
          Log.debug (fun m -> m "from [%20s] -> %a" identity pp_data data) ;
          match Bob.Relay.receive_from t ~identity data with
          | `Continue | `Read -> Fiber.return `Continue
          | `Close -> Fiber.close fd >>= fun () -> Fiber.return `Delete
          | `Agreement _ ->
            (* XXX(dinosaure): In such case, the peer is a client and it sent
               an [Accepted] packet. *)
            full_read fd >>= fun data ->
            Log.debug (fun m -> m "Remaining data from %s: %a"
              identity pp_data data) ;
            let _continue_or_read = Bob.Relay.receive_from t ~identity data in
            Fiber.close fd >>= fun () ->
            Fiber.return `Delete end in
        Some (fd, ivar) end fds ; 
      Fiber.pause () >>= read in
  let handler fd sockaddr =
    let identity = Fmt.str "%a" pp_sockaddr sockaddr in
    Log.debug (fun m -> m "Add %s as an active connection." identity) ;
    let ivar = Fiber.Ivar.full `Continue in
    Hashtbl.add fds identity (fd, ivar) ;
    Bob.Relay.new_peer t ~identity ;
    Fiber.async begin fun () -> Fiber.sleep timeout >>= fun () ->
    if Bob.Relay.exists t ~identity
    then ( Log.warn (fun m -> m "%s timeout" identity)
         ; Bob.Relay.rem_peer t ~identity ) ;
    Hashtbl.remove fds identity ;
    Fiber.close fd end ;
    Fiber.return () in
  fork_and_join
    begin fun () -> serve_when_ready ~stop ~handler socket end
    begin fun () -> fork_and_join read write >>= fun ((), ()) ->
          Fiber.return () end
  >>= fun ((), ()) -> Fiber.return ()
