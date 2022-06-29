let run timeout inet_addr port secure_port backlog =
  let sockaddr01 = Unix.ADDR_INET (inet_addr, port) in
  let sockaddr02 = Unix.ADDR_INET (inet_addr, secure_port) in
  let socket01 =
    Unix.socket ~cloexec:true
      (Unix.domain_of_sockaddr sockaddr01)
      Unix.SOCK_STREAM 0
  in
  let socket02 =
    Unix.socket ~cloexec:true
      (Unix.domain_of_sockaddr sockaddr02)
      Unix.SOCK_STREAM 0
  in
  let secured = Bob_unix.create_secure_room () in
  Unix.bind socket01 sockaddr01;
  Unix.bind socket02 sockaddr02;
  Unix.listen socket01 backlog;
  Unix.listen socket02 backlog;
  let stop = Fiber.Ivar.create () in
  Sys.set_signal Sys.sigint
    (Signal_handle
       (fun _sigint ->
         Logs.debug (fun m -> m "The user wants to stop the relay.");
         if Fiber.Ivar.is_empty stop then Fiber.Ivar.fill stop ()));
  Fiber.run
    Fiber.(
      fork_and_join
        (fun () -> Bob_clear.relay ~timeout socket01 secured ~stop)
        (fun () -> Bob_unix.secure_room ~timeout socket02 secured ~stop)
      >>= fun ((), ()) -> Fiber.return ());
  Unix.close socket01;
  Unix.close socket02;
  `Ok 0

let run _quiet daemonize timeout inet_addr port secure_port backlog () =
  match daemonize with
  | Some path ->
    Daemon.daemonize ~path begin fun () ->
    run timeout inet_addr port secure_port backlog end
  | None ->
    run timeout inet_addr port secure_port backlog

open Cmdliner
open Args

let daemonize =
  let doc = "Daemonize the process and show the PID." in
  Arg.(value & opt (some string) None & info [ "daemonize" ] ~doc)

let timeout =
  let doc = "How long the relay can keep an active connection (in seconds)." in
  Arg.(value & opt float 3600. & info [ "t"; "timeout" ] ~doc ~docv:"<seconds>")

let inet_addr =
  let doc = "Set the source address where the relay will be bound." in
  Arg.(
    value
    & pos 0 inet_addr Unix.inet_addr_any
    & info [] ~doc ~docv:"<inet-addr>")

let port =
  let doc = "Set the port where the relay will listen." in
  Arg.(value & pos 1 int 9000 & info [] ~doc ~docv:"<port>")

let backlog =
  let doc =
    "The maximum length to which the queue of pending connections may grow."
  in
  Arg.(value & opt int 40 & info [ "b"; "backlog" ] ~doc ~docv:"<backlog>")

let pid =
  let doc = "Write into a file the PID of the relay." in
  let env = Cmd.Env.info "BOB_PID" in
  let fpath = Arg.conv (Fpath.of_string, Fpath.pp) in
  Arg.(
    value
    & opt (some fpath) None
    & info [ "p"; "pid" ] ~env ~doc ~docv:"<filename>")

let setup_pid = function
  | None -> ()
  | Some fpath ->
      let pid = Unix.getpid () in
      let oc = open_out_bin (Fpath.to_string fpath) in
      output_string oc (Fmt.str "%d" pid);
      close_out oc

let setup_pid = Term.(const setup_pid $ pid)

let cmd =
  let doc = "Launch the Bob relay." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) launches a Bob relay which is able to handle peers and let \
         them to do handshakes and transfer files. The relay can be safely \
         killed by a $(b,SIGINT) (^C) signal.";
    ]
  in
  Cmd.v
    (Cmd.info "relay" ~doc ~man)
    Term.(
      ret
        (const run $ setup_logs $ daemonize $ timeout $ inet_addr $ port $ secure_port
       $ backlog $ setup_pid))
