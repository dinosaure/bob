let () = Printexc.record_backtrace true

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let relay_sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9000)
let relay_domain = Unix.domain_of_sockaddr relay_sockaddr

let rec connect fd sockaddr =
  try Unix.connect fd sockaddr ; Ok ()
  with Unix.Unix_error (Unix.EINTR, _, _) -> connect fd sockaddr
     | Unix.Unix_error (Unix.EACCES, _, _) ->
       ( match sockaddr with
       | Unix.ADDR_UNIX _ -> Error `Permission_denied
       | Unix.ADDR_INET _ -> Error `Access_denied )
     | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
       Error `Local_address_already_in_use
     | Unix.Unix_error (Unix.EADDRNOTAVAIL, _, _) ->
       Error `Address_not_available
     | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
       Error `Connection_refused
     | Unix.Unix_error (Unix.ENETUNREACH, _, _) ->
       Error `Network_unreachable
     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
       Error `Timeout
     | Unix.Unix_error (errno, f, arg) ->
       Error (`Unix (errno, f, arg))

let pp_error ppf = function
  | `Permission_denied -> Fmt.string ppf "Permission denied"
  | `Access_denied -> Fmt.string ppf "Access denied"
  | `Local_address_already_in_use -> Fmt.string ppf "Local address already in use"
  | `Address_not_available -> Fmt.string ppf "Address not available"
  | `Connection_refused -> Fmt.string ppf "Connection refused"
  | `Network_unreachable -> Fmt.string ppf "Network unreachable"
  | `Timeout -> Fmt.string ppf "Timeout"
  | `Unix (errno, f, arg) ->
    Fmt.pf ppf "%s(%s): %s" f arg (Unix.error_message errno)

let run_client password =
  let g = Random.State.make_self_init () in
  let socket = Unix.socket ~cloexec:true relay_domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  connect socket relay_sockaddr |> function
  | Error err ->
    Fmt.epr "%s: %a.\n%!" Sys.executable_name pp_error err ;
    Fiber.return ()
  | Ok () ->
    Logs.debug (fun m -> m "The client is connected to the relay.") ;
    let choose = Fiber.Ivar.full `Accept in
    Bob_unix.client socket ~choose ~g ~password >>= function
    | Ok _shared_keys -> Fiber.return ()
    | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_unix.pp_error err ;
      Fiber.return ()

let run_client password = Fiber.run (run_client password)

let run_server password =
  let g = Random.State.make_self_init () in
  let socket = Unix.socket ~cloexec:true relay_domain Unix.SOCK_STREAM 0 in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let open Fiber in
  connect socket relay_sockaddr |> function
  | Error err ->
    Fmt.epr "%s: %a.\n%!" Sys.executable_name pp_error err ;
    Fiber.return ()
  | Ok () ->
    Bob_unix.server socket ~g ~secret >>= function
    | Ok _shared_keys -> Fiber.return ()
    | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_unix.pp_error err ;
      Fiber.return ()

let run_server password = Fiber.run (run_server password)

let run_relay () =
  let socket = Unix.socket ~cloexec:true relay_domain Unix.SOCK_STREAM 0 in
  Unix.bind socket relay_sockaddr ;
  Unix.listen socket 40 ;
  let stop = Fiber.Ivar.create () in
  Sys.set_signal Sys.sigint (Signal_handle begin fun _sigint ->
  Logs.debug (fun m -> m "The user wants to stop the relay.") ;
  if Fiber.Ivar.is_empty stop then Fiber.Ivar.fill stop () end) ;
  Fiber.run (Bob_unix.relay socket ~stop)

let () = match Sys.argv with
  | [| _; "client"; password |] -> run_client password
  | [| _; "server"; password |] -> run_server password
  | [| _; "relay"; |] -> run_relay ()
  | _ ->
    Fmt.epr "%s [client|server] <password>\n%!" Sys.executable_name
