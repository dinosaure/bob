let flip (a, b) = (b, a)

let make_progress_bar ~total =
  let open Progress.Line in
  list [ bar total; count_to total ]

let pack ~g store hashes reporter =
  let reporter n =
    Fmt.epr ">>> reporter %d.\n%!" n;
    reporter n;
    Fiber.return ()
  in
  Pack.make ~g ~reporter store hashes

let run_server g sockaddr secure_port password path =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let open Fiber in
  let hashes, store = Pack.store path in
  let config = Progress.Config.v ~ppf:Fmt.stdout () in
  Progress.with_reporter ~config
    (make_progress_bar ~total:(List.length hashes))
    (pack ~g store hashes)
  >>= fun () ->
  Connect.connect socket sockaddr |> function
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Connect.pp_error err;
      Fiber.return 1
  | Ok () -> (
      Bob_clear.server socket ~g ~secret >>= function
      | Ok (identity, ciphers, shared_keys) ->
          Fmt.pr "Handshake done with %s.\n%!" identity;
          Chat.chat
            (Chat.sockaddr_with_secure_port sockaddr secure_port)
            ~identity ~ciphers:(flip ciphers) ~shared_keys:(flip shared_keys)
      | Error err ->
          Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_clear.pp_error err;
          Fiber.return 1)

let run _quiet g () sockaddr secure_port password path =
  let code = Fiber.run (run_server g sockaddr secure_port password path) in
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
  Arg.(
    value
    & opt (some string) None
    & info [ "p"; "password" ] ~doc ~docv:"<password>")

let path =
  let doc = "Document to archive." in
  let existing_object =
    let parser str =
      match Fpath.of_string str with
      | Ok v when Sys.file_exists str -> Ok v
      | Ok v -> Error (`Msg (Fmt.str "%a does not exist" Fpath.pp v))
      | Error _ as err -> err
    in
    Arg.conv (parser, Fpath.pp)
  in
  Arg.(
    required & pos 0 (some existing_object) None & info [] ~doc ~docv:"<path>")

let cmd =
  let doc = "Send a file to a peer who share the given password." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) tries many handshakes with many peers throught the given \
         relay and the given password. Once found, it sends the file to the\n\
        \          peer.";
    ]
  in
  Cmd.v
    (Cmd.info "send" ~doc ~man)
    Term.(
      ret
        (const run $ setup_logs $ setup_random $ setup_temp $ relay
       $ secure_port $ setup_password password $ path))
