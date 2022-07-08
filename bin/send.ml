open Stdbob
open Prgrss

let compress_with_reporter quiet ~compression ~config store hashes =
  with_reporter ~config quiet
    (make_compression_progress ~total:(Pack.length store))
  @@ fun (reporter, finalise) ->
  let open Fiber in
  Pack.deltify ~reporter ~compression store hashes >>| fun res ->
  finalise ();
  res

let emit_with_reporter quiet ?g ?level ~config store objects =
  with_reporter ~config quiet (make_progress_bar ~total:(Pack.length store))
  @@ fun (reporter, finalise) ->
  let open Fiber in
  let open Stream in
  let path = Temp.random_temporary_path ?g "pack-%s.pack" in
  let flow = Pack.make ?level ~reporter:(fun () -> reporter 1) store in
  Stream.(to_file path (via flow objects)) >>= fun () ->
  finalise ();
  Fiber.return path

let transfer_with_reporter quiet ~config ~identity ~ciphers ~shared_keys
    sockaddr fpath =
  let total = (Unix.stat (Fpath.to_string fpath)).Unix.st_size in
  with_reporter ~config quiet (make_tranfer_bar ~total)
  @@ fun (reporter, finalise) ->
  let open Fiber in
  Transfer.transfer ~reporter ~identity ~ciphers ~shared_keys sockaddr fpath
  >>| fun res ->
  finalise ();
  res

let run_server quiet g compression sockaddr secure_port password path =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let config = Progress.Config.v ~ppf:Fmt.stdout () in
  let open Fiber in
  Pack.store path >>= fun (hashes, store) ->
  match Pack.length store with
  | 0 ->
      assert false
      (* XXX(dinosaure): impossible case. The user gaves
         a valid path which contains, at least, one object. *)
  | 1 -> assert false
  | n -> (
      if not quiet then Fmt.pr ">>> %*s%d object(s).\n%!" 33 " " n;
      compress_with_reporter quiet ~config ~compression store hashes
      >>= fun compressed ->
      emit_with_reporter quiet ~g
        ~level:(if compression then 4 else 0)
        ~config store compressed
      >>= fun fpath ->
      Connect.connect socket sockaddr |> function
      | Error err ->
          Fmt.epr "%s: %a.\n%!" Sys.executable_name Connect.pp_error err;
          Fiber.return 1
      | Ok () -> (
          Bob_clear.server socket ~g ~secret >>= function
          | Ok (identity, ciphers, shared_keys) -> (
              let sockaddr =
                Transfer.sockaddr_with_secure_port sockaddr secure_port
              in
              transfer_with_reporter quiet ~config ~identity
                ~ciphers:(flip ciphers) ~shared_keys:(flip shared_keys) sockaddr
                fpath
              >>= function
              | Ok () -> Fiber.return 0
              | Error err ->
                  Fmt.epr "%s: %a.\n%!" Sys.executable_name Transfer.pp_error
                    err;
                  Fiber.return 1)
          | Error err ->
              Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_clear.pp_error err;
              Fiber.return 1))

let run quiet g () compression sockaddr secure_port password path =
  let code =
    Fiber.run
      (run_server quiet g compression sockaddr secure_port password path)
  in
  `Ok code

open Cmdliner
open Args

let relay =
  let doc = "The IP address of the relay." in
  Arg.(
    value
    & opt (addr_inet ~default:9000) Unix.(ADDR_INET (inet_addr_loopback, 9000))
    & info [ "r"; "relay" ] ~doc ~docv:"<addr>:<port>")

let compression =
  Arg.(
    value
    & vflag true
        [
          (true, info [ "compress" ] ~doc:"Explicitly compress objects");
          ( false,
            info [ "no-compression" ]
              ~doc:
                "Explicitly store objects as they are (useful for video/image)"
          );
        ])

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
         relay and the given password. Once found, it sends the file to the \
         peer.";
    ]
  in
  Cmd.v
    (Cmd.info "send" ~doc ~man)
    Term.(
      ret
        (const run $ setup_logs $ setup_random $ setup_temp $ compression
       $ relay $ secure_port $ setup_password password $ path))
