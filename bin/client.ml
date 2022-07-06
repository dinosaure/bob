let choose = function
  | true -> fun _identity -> Fiber.return `Accept
  | false ->
      let open Fiber in
      fun identity ->
        let rec asking () =
          Fiber.getline Unix.stdin >>| Option.map String.lowercase_ascii
          >>= function
          | Some ("y" | "yes" | "") | None -> Fiber.return `Accept
          | Some ("n" | "no") -> Fiber.return `Refuse
          | _ ->
              Fmt.pr "Invalid response, accept from %s [Y/n]: %!" identity;
              asking ()
        in
        Fmt.pr "Accept from %s [Y/n]: %!" identity;
        asking ()

let incoming_data =
  let open Progress.Line in
  let spin =
    spinner
      ~frames:
        [
          "⠁";
          "⠉";
          "⠙";
          "⠚";
          "⠒";
          "⠂";
          "⠂";
          "⠒";
          "⠲";
          "⠴";
          "⠤";
          "⠄";
          "⠄";
          "⠤";
          "⠴";
          "⠲";
          "⠒";
          "⠂";
          "⠂";
          "⠒";
          "⠚";
          "⠙";
          "⠉";
          "⠁";
        ]
      ()
  in
  list [ spin; bytes; bytes_per_sec ]

let save_with_reporter quiet ~config ?g ~identity ~ciphers ~shared_keys sockaddr
    =
  let reporter, finalize =
    match quiet with
    | true -> (ignore, ignore)
    | false ->
        let display =
          Progress.Multi.line incoming_data |> Progress.Display.start ~config
        in
        let[@warning "-8"] Progress.Reporter.[ reporter ] =
          Progress.Display.reporters display
        in
        ( (fun n ->
            reporter n;
            Progress.Display.tick display),
          fun () -> Progress.Display.finalise display )
  in
  let open Fiber in
  Transfer.save ?g ~reporter ~identity ~ciphers ~shared_keys sockaddr
  >>| fun res ->
  finalize ();
  res

let run_client quiet g sockaddr secure_port password yes =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Connect.connect socket sockaddr |> function
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Connect.pp_error err;
      Fiber.return 1
  | Ok () -> (
      Logs.debug (fun m -> m "The client is connected to the relay.");
      let choose = choose yes in
      Bob_clear.client socket ~choose ~g ~password >>= function
      | Ok (identity, ciphers, shared_keys) -> (
          let config = Progress.Config.v ~ppf:Fmt.stdout () in
          let sockaddr =
            Transfer.sockaddr_with_secure_port sockaddr secure_port
          in
          save_with_reporter quiet ~config ~g ~identity ~ciphers ~shared_keys
            sockaddr
          >>= function
          | Ok fpath ->
              Fmt.epr ">>> Document saved into %a.\n%!" Fpath.pp fpath;
              Fiber.return 0
          | Error err ->
              Fmt.epr "%s: %a.\n%!" Sys.executable_name Transfer.pp_error err;
              Fiber.return 1)
      | Error err ->
          Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_clear.pp_error err;
          Fiber.return 1)

let run quiet g sockaddr secure_port password yes =
  let code = Fiber.run (run_client quiet g sockaddr secure_port password yes) in
  `Ok code

open Cmdliner
open Args

let relay =
  let doc = "The IP address of the relay." in
  Arg.(
    value
    & opt (addr_inet ~default:9000) Unix.(ADDR_INET (inet_addr_loopback, 9000))
    & info [ "r"; "relay" ] ~doc ~docv:"<addr>:<port>")

let password =
  let doc = "The password to share." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"<password>")

let yes =
  let doc = "Answer yes to all bob questions without prompting." in
  Arg.(value & flag & info [ "y"; "yes" ] ~doc)

let term =
  Term.(
    ret
      (const run $ setup_logs $ setup_random $ relay $ secure_port $ password
     $ yes))

let cmd =
  let doc = "Receive a file from a peer who share the given password." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) tries many handshakes with many peers throught the given \
         relay with the given password. Once found, it asks the user if it \
         wants to complete the handshake. Therefore, if the user accepts, we \
         receive the desired file. Otherwise, $(tname) waits for another peer.";
    ]
  in
  Cmd.v (Cmd.info "recv" ~doc ~man) term
