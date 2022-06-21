let choose = function
  | true  -> begin fun _identity -> Fiber.return `Accept end
  | false ->
    let open Fiber in
    begin fun  identity ->
    let rec asking () =
      Fiber.getline Unix.stdin >>| Option.map String.lowercase_ascii >>= function
      | Some ("y" | "yes" | "") | None -> Fiber.return `Accept
      | Some ("n" | "no")              -> Fiber.return `Refuse
      | _ ->
        Fmt.pr "Invalid response, accept from %s [Y/n]: %!" identity ;
        asking () in
    Fmt.pr "Accept from %s [Y/n]: %!" identity ; asking () end

let run_client g sockaddr password yes =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Connect.connect socket sockaddr |> function
  | Error err ->
    Fmt.epr "%s: %a.\n%!" Sys.executable_name Connect.pp_error err ;
    Fiber.return 1
  | Ok () ->
    Logs.debug (fun m -> m "The client is connected to the relay.") ;
    let choose = choose yes in
    Bob_unix.client socket ~choose ~g ~password >>= function
    | Ok (`Accepted_with (identity, _shared_keys)) ->
      Fmt.pr "Handshake done with %s.\n%!" identity ;
      Fiber.return 0
    | Ok `Refused -> Fiber.return 0
    | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_unix.pp_error err ;
      Fiber.return 1

let run _quiet g sockaddr password yes =
  let code = Fiber.run (run_client g sockaddr password yes) in
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

let yes =
  let doc = "Answer yes to all bob questions without prompting." in
  Arg.(value & flag & info ["y"; "yes"] ~doc)

let cmd =
  let doc = "Receive a file from a peer who share the given password." in
  let man =
    [ `S Manpage.s_description
    ; `P "$(tname) tries many handshakes with many peers throught the given \
          relay and the given password. Once found, we receive the desired \
          file." ] in
  Cmd.v (Cmd.info "recv" ~doc ~man)
    Term.(ret (const run $ setup_logs $ setup_random $ relay $ password $ yes)) 
