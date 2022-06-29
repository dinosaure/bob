let daemonize ~path prgm =
  match Unix.fork () with
  | 0 ->
    ( Sys.set_signal Sys.sigchld Sys.Signal_ignore
    ; let _ = Unix.setsid () in
      match Unix.fork () with
    | 0 ->
      let _ = Unix.umask 0 in
      Unix.chdir path ; prgm ()
    | pid -> Fmt.pr "%d\n%!" pid ; exit 0 )
  | _pid -> exit 0
