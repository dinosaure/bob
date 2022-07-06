let flip (a, b) = (b, a)

let make_compression_progress ~total =
  let open Progress.Line in
  list [ spacer 32; count_to total; const "delta-ified object(s)" ]

let make_progress_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      count_to total;
      const "compressed object(s)";
    ]

let make_tranfer_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list [ brackets @@ bar ~style ~width:(`Fixed 30) total; percentage_of total ]

let compress_with_reporter quiet ~config store hashes =
  let reporter, finalise =
    match quiet with
    | true -> ((fun _ -> Fiber.return ()), ignore)
    | false ->
        let display =
          Progress.Multi.line
            (make_compression_progress ~total:(List.length hashes))
          |> Progress.Display.start ~config
        in
        let[@warning "-8"] Progress.Reporter.[ reporter ] =
          Progress.Display.reporters display
        in
        ( (fun n ->
            reporter n;
            Progress.Display.tick display;
            Fiber.return ()),
          fun () -> Progress.Display.finalise display )
  in
  let open Fiber in
  Pack.deltify ~reporter store hashes >>| fun res ->
  finalise ();
  res

let emit_with_reporter quiet ?g ?level ~config store compressed =
  let reporter, finalise =
    match quiet with
    | true -> ((fun _ -> Fiber.return ()), ignore)
    | false ->
        let display =
          Progress.Multi.line
            (make_progress_bar ~total:(Array.length compressed))
          |> Progress.Display.start ~config
        in
        let[@warning "-8"] Progress.Reporter.[ reporter ] =
          Progress.Display.reporters display
        in
        ( (fun () ->
            reporter 1;
            Progress.Display.tick display;
            Fiber.return ()),
          fun () -> Progress.Display.finalise display )
  in
  let open Fiber in
  Pack.make ?g ?level ~reporter store compressed >>| fun res ->
  finalise ();
  res

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) ppf = function
  | 0 -> Fmt.string ppf "0 byte"
  | n ->
      let n = float_of_int n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Fmt.pf ppf "%.*f %s" decimals r sizes.(int_of_float i)

let summarize quiet compressed fpath =
  (if not quiet then
   let inflated =
     Array.fold_left
       (fun sum q -> sum + Carton.Enc.target_length q)
       0 compressed
   in
   let deflated = (Unix.stat (Fpath.to_string fpath)).Unix.st_size in
   let decimals = 1 in
   let a = Fmt.to_to_string (bytes_to_size ~decimals) inflated in
   let b = Fmt.to_to_string (bytes_to_size ~decimals) deflated in
   let len = String.length a + String.length b in
   let arrow = if len mod 2 = 0 then " -> " else " --> " in
   let len = len + String.length arrow in
   let pad = (30 - len) / 2 in
   Fmt.pr "[%*s%s%s%s%*s]\n%!" pad " " a arrow b pad " ");
  Fiber.return ()

let transfer_with_reporter quiet ~config ~identity ~ciphers ~shared_keys
    sockaddr fpath =
  let reporter, finalize =
    match quiet with
    | true -> (ignore, ignore)
    | false ->
        let total = (Unix.stat (Fpath.to_string fpath)).Unix.st_size in
        let display =
          Progress.Multi.line (make_tranfer_bar ~total)
          |> Progress.Display.start ~config
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
  Transfer.transfer ~reporter ~identity ~ciphers ~shared_keys sockaddr fpath
  >>| fun res ->
  finalize ();
  res

let run_server quiet g compression sockaddr secure_port password path =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
  let open Fiber in
  let hashes, store = Pack.store path in
  let config = Progress.Config.v ~ppf:Fmt.stdout () in
  compress_with_reporter quiet ~config store hashes >>= fun compressed ->
  emit_with_reporter quiet ~g
    ~level:(if compression then 4 else 0)
    ~config store compressed
  >>= fun fpath ->
  summarize quiet compressed fpath >>= fun () ->
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
          transfer_with_reporter quiet ~config ~identity ~ciphers:(flip ciphers)
            ~shared_keys:(flip shared_keys) sockaddr fpath
          >>= function
          | Ok () -> Fiber.return 0
          | Error err ->
              Fmt.epr "%s: %a.\n%!" Sys.executable_name Transfer.pp_error err;
              Fiber.return 1)
      | Error err ->
          Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_clear.pp_error err;
          Fiber.return 1)

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
