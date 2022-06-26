let run_server g sockaddr password =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let open Fiber in
  Connect.connect socket sockaddr |> function
  | Error err ->
    Fmt.epr "%s: %a.\n%!" Sys.executable_name Connect.pp_error err ;
    Fiber.return 1
  | Ok () ->
    Bob_clear.server socket ~g ~secret >>= function
    | Ok (identity, _shared_keys) ->
      Fmt.pr "Handshake done with %s.\n%!" identity ;
      Fiber.return 0
    | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_clear.pp_error err ;
      Fiber.return 1

let run _quiet g sockaddr password =
  let code = Fiber.run (run_server g sockaddr password) in
  `Ok code

open Cmdliner
open Args

let relay =
  let doc = "The IP address of the relay." in
  Arg.(value & opt (addr_inet ~default:9000)
                   Unix.(ADDR_INET (inet_addr_loopback, 9000))
             & info ["r"; "relay"] ~doc)

let password =
  let doc = "The password to share." in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let cmd =
  let doc = "Send a file to a peer who share the given password." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) tries many handshakes with many peers throught the given \
          relay and the given password. Once found, it sends the file to the
          peer." ] in
  Cmd.v (Cmd.info "send" ~doc ~man)
    Term.(ret (const run $ setup_logs $ setup_random $ relay $ password))
