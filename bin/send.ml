open Stdbob
open Prgrss

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore
(* XXX(dinosaure): if the receiver close abruptely the connection, a SIGPIPE
   is raised by the system to our binary. We ignore that and prefer to get
   the EPIPE error on the [write()] syscall. *)

let compress_with_reporter quiet ~compression ~config store hashes =
  with_reporter ~config quiet
    (make_compression_progress ~total:(Pack.length store))
  @@ fun (reporter, finalise) ->
  let ( let* ) = Fiber.bind in
  Logs.debug (fun m -> m "Start deltification.");
  let reporter = Fiber.return <.> reporter in
  let stream = Pack.deltify ~reporter ~compression store hashes in
  let* targets = Stream.Stream.to_list stream in
  Logs.debug (fun m -> m "Deltification is done.");
  finalise ();
  Fiber.return (Stream.Stream.of_list targets)

type pack = Stream of Stdbob.bigstring Stream.stream | File of Bob_fpath.t

let emit_with_reporter quiet ?g ?level ~config store
    (objects : _ Cartonnage.Target.t Stream.stream) =
  with_reporter ~config quiet
    (make_progress_bar_for_objects ~total:(Pack.length store))
  @@ fun (reporter, finalise) ->
  let ( let* ) = Fiber.bind in
  let open Stream in
  let path = Temp.random_temporary_path ?g "pack-%s.pack" in
  Logs.debug (fun m -> m "Generate the PACK file: %a" Bob_fpath.pp path);
  let flow =
    Pack.make ?level
      ~reporter:(Fiber.return <.> reporter <.> Stdbob.always 1)
      store
  in
  let flow = Flow.(flow << bigstring_to_string) in
  let* () = Stream.(to_file path (via flow objects)) in
  finalise ();
  Fiber.return (File path)

let emit_one_with_reporter quiet ?level ~config path =
  let total = Unix.(stat (Bob_fpath.to_string path)).Unix.st_size in
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
      let stream = Stream.(Stream.via Flow.bigstring_to_string stream) in
      Transfer.transfer ~identity ~ciphers ~shared_keys sockaddr stream
  | File path ->
      let total = (Unix.stat (Bob_fpath.to_string path)).Unix.st_size in
      with_reporter ~config quiet (make_tranfer_bar ~total)
      @@ fun (reporter, finalise) ->
      let open Fiber in
      let open Stream in
      let bstr =
        Bigarray.Array1.create Bigarray.char Bigarray.c_layout
          Bob_unix.Crypto.max_packet
      in
      let fn str =
        let src_off = 0 and dst_off = 0 and len = String.length str in
        Stdbob.bigstring_blit_from_string str ~src_off bstr ~dst_off ~len;
        bstr
      in
      Stream.of_file ~len:Bob_unix.Crypto.max_packet ~fn path >>| Result.get_ok
      >>= fun stream ->
      let stream = Stream.via Flow.bigstring_to_string stream in
      Transfer.transfer
        ~reporter:(Fiber.return <.> reporter)
        ~identity ~ciphers ~shared_keys sockaddr stream
      >>| fun res ->
      finalise ();
      res

let generate_pack_file quiet ~config ~g compression path =
  let open Fiber in
  Pack.store path >>= fun (uids, store) ->
  match Pack.length store with
  | 0 -> assert false
  | 1 ->
      emit_one_with_reporter quiet
        ~level:(if compression then 4 else 0)
        ~config path
  | n ->
      if not quiet then Fmt.pr ">>> %*s%d objets.\n%!" 33 " " n;
      compress_with_reporter quiet ~config ~compression store uids
      >>= fun compressed ->
      emit_with_reporter quiet ~g
        ~level:(if compression then 4 else 0)
        ~config store compressed

(* XXX(dinosaure): we don't want to compress for video, audio and image. *)
let better_to_compress_for mime_type =
  match String.split_on_char '/' mime_type with
  | "video" :: _ | "audio" :: _ | "image" :: _ -> false
  | _ -> true

let run_server quiet g (_, he) mime_type compression addr secure_port reproduce
    password path =
  let compression =
    match (mime_type, compression) with
    | None, None -> true
    | Some mime_type, None -> better_to_compress_for mime_type
    | _, Some v -> v
  in
  let config = Progress.Config.v ~ppf:Fmt.stdout () in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let open Fiber in
  Bob_happy_eyeballs.connect he addr >>? fun (sockaddr, socket) ->
  generate_pack_file quiet ~g ~config compression path >>= fun pack ->
  Bob_clear.server socket ~reproduce ~g secret
  >>? fun (identity, ciphers, shared_keys) ->
  let sockaddr = Transfer.sockaddr_with_secure_port sockaddr secure_port in
  transfer_with_reporter quiet ~config ~identity ~ciphers:(flip ciphers)
    ~shared_keys:(flip shared_keys) sockaddr pack
  >>| Transfer.open_error

let pp_error ppf = function
  | #Transfer.error as err -> Transfer.pp_error ppf err
  | #Bob_clear.error as err -> Bob_clear.pp_error ppf err
  | `Msg err -> Fmt.pf ppf "%s" err

let run () dns_and_he mime_type compression addr secure_port reproduce
    (quiet, g, password) path =
  match
    Fiber.run
      (run_server quiet g dns_and_he mime_type compression addr secure_port
         reproduce password path)
  with
  | Ok () -> `Ok 0
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.argv.(0) pp_error err;
      `Ok 1

open Cmdliner
open Args

let mime_type =
  let doc =
    "The MIME type of the document. You can use the $(b,file) command to \
     recognize the MIME type of a document. If the document is recognized as a \
     video, an audio or an image, we will disable the compression."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "mime-type" ] ~doc ~docv:"<mime-type>")

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
      match Bob_fpath.of_string str with
      | Ok v when Sys.file_exists str -> Ok v
      | Ok v -> Error (`Msg (Fmt.str "%a does not exist" Bob_fpath.pp v))
      | Error _ as err -> err
    in
    Arg.conv (parser, Bob_fpath.pp)
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
        (const run $ term_setup_temp $ term_setup_dns $ mime_type $ compression
       $ relay $ secure_port $ reproduce
        $ term_setup_password password
        $ path))
