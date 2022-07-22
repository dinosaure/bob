open Stdbob
open Prgrss

let compress_with_reporter quiet ~compression ~config store hashes =
  with_reporter ~config quiet
    (make_compression_progress ~total:(Pack.length store))
  @@ fun (reporter, finalise) ->
  let open Fiber in
  Logs.debug (fun m -> m "Start deltification.");
  Pack.deltify ~reporter:(Fiber.return <.> reporter) ~compression store hashes
  >>| fun res ->
  Logs.debug (fun m -> m "Deltification is done.");
  finalise ();
  res

type pack = Stream of Stdbob.bigstring Stream.stream | File of Fpath.t

let emit_with_reporter quiet ?g ?level ~config store
    (objects : Digestif.SHA1.t Carton.Enc.q Stream.stream) =
  with_reporter ~config quiet
    (make_progress_bar_for_objects ~total:(Pack.length store))
  @@ fun (reporter, finalise) ->
  let open Fiber in
  let open Stream in
  let path = Temp.random_temporary_path ?g "pack-%s.pack" in
  Logs.debug (fun m -> m "Generate the PACK file: %a" Fpath.pp path);
  let flow =
    Pack.make ?level
      ~reporter:(Fiber.return <.> reporter <.> Stdbob.always 1)
      store
  in
  Stream.(to_file path (via flow objects)) >>= fun () ->
  finalise ();
  Fiber.return (File path)

let emit_one_with_reporter quiet ?level ~config path =
  let total = Unix.(stat (Fpath.to_string path)).Unix.st_size in
  with_reporter ~config quiet (make_progress_bar_for_file ~total)
  @@ fun (reporter, finalise) ->
  let open Fiber in
  Pack.make_one ?level ~reporter:(Fiber.return <.> reporter) ~finalise path
  >>= function
  | Error (`Msg err) -> Fmt.failwith "%s." err
  | Ok stream -> Fiber.return (Stream stream)

let transfer_with_reporter quiet ~config ~identity ~ciphers ~shared_keys
    sockaddr = function
  | Stream stream ->
      Transfer.transfer ~identity ~ciphers ~shared_keys sockaddr stream
  | File path ->
      let total = (Unix.stat (Fpath.to_string path)).Unix.st_size in
      with_reporter ~config quiet (make_tranfer_bar ~total)
      @@ fun (reporter, finalise) ->
      let open Fiber in
      let open Stream in
      Stream.of_file path >>| Result.get_ok
      >>= Transfer.transfer
            ~reporter:(Fiber.return <.> reporter)
            ~identity ~ciphers ~shared_keys sockaddr
      >>| fun res ->
      finalise ();
      res

let generate_pack_file quiet ~config ~g compression path =
  let open Fiber in
  Pack.store path >>= fun (hashes, store) ->
  match Pack.length store with
  | 0 -> assert false
  | 1 ->
      emit_one_with_reporter quiet
        ~level:(if compression then 4 else 0)
        ~config path
  | n ->
      if not quiet then Fmt.pr ">>> %*s%d objets.\n%!" 33 " " n;
      compress_with_reporter quiet ~config ~compression store hashes
      >>= fun compressed ->
      emit_with_reporter quiet ~g
        ~level:(if compression then 4 else 0)
        ~config store compressed

let run_server quiet g compression addr secure_port password path =
  let sockaddr = match addr with
    | `Inet (inet_addr, port) -> Unix.ADDR_INET (inet_addr, port)
    | _ -> assert false in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let config = Progress.Config.v ~ppf:Fmt.stdout () in
  let open Fiber in
  generate_pack_file quiet ~g ~config compression path >>= fun pack ->
  Fiber.connect socket sockaddr >>| reword_error (fun errno -> `Connect errno)
  >>? fun () ->
  Bob_clear.server socket ~g ~secret >>? fun (identity, ciphers, shared_keys) ->
  let sockaddr = Transfer.sockaddr_with_secure_port sockaddr secure_port in
  transfer_with_reporter quiet ~config ~identity ~ciphers:(flip ciphers)
    ~shared_keys:(flip shared_keys) sockaddr pack
  >>| Transfer.open_error

let pp_error ppf = function
  | `Blocking_connect err -> Connect.pp_error ppf err
  | #Transfer.error as err -> Transfer.pp_error ppf err
  | #Bob_clear.error as err -> Bob_clear.pp_error ppf err

let run quiet g () compression sockaddr secure_port password path =
  match
    Fiber.run
      (run_server quiet g compression sockaddr secure_port password path)
  with
  | Ok () -> `Ok 0
  | Error err ->
      Fmt.epr "%s: %a\n%!" Sys.executable_name pp_error err;
      `Ok 1

open Cmdliner
open Args

let relay =
  let doc = "The address of the relay." in
  Arg.(
    value
    & opt (addr ~default:9000) (`Inet (Unix.inet_addr_loopback, 9000))
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
