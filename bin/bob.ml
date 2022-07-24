let () = Printexc.record_backtrace true

open Cmdliner

let () =
  let doc = "An universal & secure peer-to-peer file-transfer program." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a simple program to transfer a file from one to another. \
         It requires a $(b,relay).";
    ]
  in
  let cmd =
    Cmd.group ~default:Recv.term
      (Cmd.info "bob" ~version:"%%VERSION%%" ~doc ~man)
      [ Relay.cmd; Recv.cmd; Send.cmd ]
  in
  exit @@ Cmd.eval' cmd
