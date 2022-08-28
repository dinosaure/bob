let () = Printexc.record_backtrace true

let run quiet g temp dns compression addr secure_port yes = function
  | Some path when Sys.file_exists path ->
      let password = Args.setup_password quiet g None in
      Send.run temp dns compression addr secure_port password (Bob_fpath.v path)
  | Some password ->
      Recv.run quiet g temp dns addr secure_port (Some password) yes
  | None -> Recv.run quiet g temp dns addr secure_port None yes

open Cmdliner
open Args

let path_or_password =
  let doc = "A password or a path to send to a peer." in
  Arg.(value & pos 0 (some string) None & info [] ~doc)

let term =
  Term.(
    ret
      (const run $ term_setup_logs $ term_setup_random $ term_setup_temp
     $ term_setup_dns $ compression $ relay $ secure_port $ yes
     $ path_or_password))

let cmd =
  let doc = "An universal & secure peer-to-peer file-transfer program." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) is a simple program to transfer a file from one to another. \
         It requires a $(b,relay).";
    ]
  in
  Cmd.v (Cmd.info "bob" ~version:"%%VERSION%%" ~doc ~man) term

let array_nth argv n =
  if Array.length argv >= n + 1 then Some argv.(n) else None

let () =
  match array_nth Sys.argv 1 with
  | Some "send" ->
      let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
      argv.(0) <- Sys.argv.(0);
      exit @@ Cmd.eval' ~argv Send.cmd
  | Some "recv" ->
      let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
      argv.(0) <- Sys.argv.(0);
      exit @@ Cmd.eval' ~argv Recv.cmd
  | Some "relay" ->
      let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
      argv.(0) <- Sys.argv.(0);
      exit @@ Cmd.eval' ~argv Relay.cmd
  | None | Some _ -> exit @@ Cmd.eval' ~argv:Sys.argv cmd
