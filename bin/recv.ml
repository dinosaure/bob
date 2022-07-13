open Stdbob
open Prgrss

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

let save_with_reporter quiet ~config ?g ~identity ~ciphers ~shared_keys sockaddr
    =
  with_reporter ~config quiet incoming_data @@ fun (reporter, finalise) ->
  let open Fiber in
  Transfer.save ?g
    ~reporter:(Fiber.return <.> reporter)
    ~identity ~ciphers ~shared_keys sockaddr
  >>| fun res ->
  finalise ();
  res

let extract_with_reporter quiet ~config path =
  with_reporter ~config quiet counter @@ fun (reporter, finalise) ->
  let open Fiber in
  let entries =
    Pack.first_pass
      ~reporter:(Fiber.return <.> reporter)
      (Stream.Source.file path)
  in
  Pack.collect entries >>= fun (matrix, oracle) ->
  finalise ();
  let total = Array.length matrix in
  with_reporter ~config quiet (make_verify_bar ~total)
  @@ fun (reporter, finalise) ->
  Pack.verify ~reporter:(reporter <.> Stdbob.always 1) ~oracle path matrix
  >>= fun () ->
  finalise ();
  let entries = Array.to_list matrix in
  let resolved, unresolved = List.partition Pack.is_resolved entries in
  match (resolved, unresolved) with
  | [ file; name ], [] when Pack.kind_of_status name = `D ->
      Pack.extract_file ~file ~name path >>= fun () -> Fiber.return (Ok ())
  | _, [] -> assert false
  | _, _unresolved -> Fiber.return (Error `Incomplete_pack_file)

let run_client quiet g sockaddr secure_port password yes =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Fiber.connect socket sockaddr >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  Logs.debug (fun m -> m "The client is connected to the relay.");
  let choose = choose yes in
  Bob_clear.client socket ~choose ~g ~password
  >>? fun (identity, ciphers, shared_keys) ->
  let config = Progress.Config.v ~ppf:Fmt.stdout () in
  let sockaddr = Transfer.sockaddr_with_secure_port sockaddr secure_port in
  save_with_reporter quiet ~config ~g ~identity ~ciphers ~shared_keys sockaddr
  >>| Transfer.open_error
  >>? fun path -> extract_with_reporter quiet ~config path

let pp_error ppf = function
  | #Transfer.error as err -> Transfer.pp_error ppf err
  | #Bob_clear.error as err -> Bob_clear.pp_error ppf err
  | `Incomplete_pack_file -> Fmt.pf ppf "Your received an incomplete PACK file"

let run quiet g sockaddr secure_port password yes =
  match Fiber.run (run_client quiet g sockaddr secure_port password yes) with
  | Ok () -> `Ok 0
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name pp_error err;
      `Ok 1

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
