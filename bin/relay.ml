let run _quiet timeout inet_addr port backlog () =
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let domain   = Unix.domain_of_sockaddr sockaddr in
  let socket   = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  Unix.bind socket sockaddr ;
  Unix.listen socket backlog ;
  let stop = Fiber.Ivar.create () in
  Sys.set_signal Sys.sigint (Signal_handle begin fun _sigint ->
  Logs.debug (fun m -> m "The user wants to stop the relay.") ;
  if Fiber.Ivar.is_empty stop then Fiber.Ivar.fill stop () end) ;
  Fiber.run (Bob_unix.relay ~timeout socket ~stop) ;
  Unix.close socket ; `Ok 0

open Cmdliner
open Args

let timeout =
  let doc = "How long the relay can keep an active connection." in
  Arg.(value & opt float 5.0 & info [ "t"; "timeout" ] ~doc)

let inet_addr =
  let doc = "Set the source address where the relay will be bound." in
  Arg.(value & pos 0 inet_addr Unix.inet_addr_any & info [] ~doc)

let port =
  let doc = "Set the port where the relay will listen." in
  Arg.(value & pos 1 int 9000 & info [] ~doc)

let backlog =
  let doc = "The maximum length to which the queue of pending connections may grow." in
  Arg.(value & opt int 40 & info [ "b"; "backlog" ] ~doc)

let pid =
  let doc = "Write into a file the PID of the relay." in
  let fpath = Arg.conv (Fpath.of_string, Fpath.pp) in
  Arg.(value & opt (some fpath) None & info [ "p"; "pid" ] ~doc)

let setup_pid = function
  | None -> ()
  | Some fpath ->
    let pid = Unix.getpid () in
    let oc = open_out_bin (Fpath.to_string fpath) in
    output_string oc (Fmt.str "%d" pid) ; close_out oc

let setup_pid = Term.(const setup_pid $ pid)

let cmd =
  let doc = "Launch the Bob relay." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) launches a Bob relay which is able to handle peers and let \
          them to do handshakes and transfer files." ] in
  Cmd.v (Cmd.info "relay" ~doc ~man)
    Term.(ret (const run $ setup_logs $ timeout $ inet_addr $ port $ backlog $ setup_pid))
