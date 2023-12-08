let () = Printexc.record_backtrace true

let run quiet g temp dns compression addr secure_port yes dst = function
  | Some path when Sys.file_exists path ->
      let password = Args.setup_password quiet g None in
      Send.run temp dns None compression addr secure_port false password
        (Bob_fpath.v path)
  | Some password ->
      Recv.run quiet g temp dns addr secure_port false (Some password) yes dst
  | None -> Recv.run quiet g temp dns addr secure_port false None yes dst

open Cmdliner
open Args

let path_or_password =
  let doc = "A password or a path to send to a peer." in
  Arg.(value & pos 0 (some string) None & info [] ~doc)

let term =
  Term.(
    ret
      (const run $ term_setup_logs $ term_setup_random $ term_setup_temp
     $ term_setup_dns $ compression $ relay $ secure_port $ yes $ destination
     $ path_or_password))

let cmd =
  let doc = "An universal & secure peer-to-peer file-transfer program." in
  let man =
    [
      `S Manpage.s_synopsis;
      `P
        "$(tname) is a simple program to transfer a file from one to another. \
         It requires a $(b,relay). It's very easy to use. It consists of two \
         sub-commands, $(b,recv) and $(b,send). Otherwise, $(tname) expects 1 \
         argument, which can be either a password (to receive something) or a \
         file / folder (to send it).";
      `S Manpage.s_description;
      `P
        "If you wish to receive a file, you must be in possession of a \
         password given by the person who wishes to transmit his or her file. \
         You can then launch $(tname) in this way to receive the file.";
      `Pre "\\$ $(tname) bryank-soindentazy";
      `P
        "You can use various options, such as automatically accepting the file \
         ($(b,-y)), defining a specific destination ($(b,-o)) or specifying a \
         relay other than the default one.";
      `Pre "\\$ $(tname) -r bob.relay.com -y -o file.txt bryank-soindentazy";
      `P
        "If you want to send a document, simply specify its location as \
         follows:";
      `Pre "\\$ $(tname) file.txt";
      `P
        "You can choose whether or not to compress the document. For images \
         and videos, we advise you $(b,not) to compress.";
      `Pre "\\$ $(tname) --no-compression Le.Sens.de.la.Fete.2017.FRENCH.mkv";
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
