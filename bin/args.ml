open Cmdliner

let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let ( <.> ) f g x = f (g x)
let common_options = "COMMON OPTIONS"

let verbosity =
  let env = Cmd.Env.info "BOB_LOGS" in
  Logs_cli.level ~docs:common_options ~env ()

let renderer =
  let env = Cmd.Env.info "BOB_FMT" in
  Fmt_cli.style_renderer ~docs:common_options ~env ()

let utf_8 =
  let doc = "Allow us to emit UTF-8 characters." in
  let env = Cmd.Env.info "BOB_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

let app_style = `Cyan
let err_style = `Red
let warn_style = `Yellow
let info_style = `Blue
let debug_style = `Green

let pp_header ~pp_h ppf (l, h) =
  match l with
  | Logs.Error ->
      pp_h ppf err_style (match h with None -> "ERROR" | Some h -> h)
  | Logs.Warning ->
      pp_h ppf warn_style (match h with None -> "WARN" | Some h -> h)
  | Logs.Info ->
      pp_h ppf info_style (match h with None -> "INFO" | Some h -> h)
  | Logs.Debug ->
      pp_h ppf debug_style (match h with None -> "DEBUG" | Some h -> h)
  | Logs.App -> (
      match h with
      | Some h -> Fmt.pf ppf "[%a] " Fmt.(styled app_style (fmt "%10s")) h
      | None -> ())

let pp_header =
  let pp_h ppf style h = Fmt.pf ppf "[%a]" Fmt.(styled style (fmt "%10s")) h in
  pp_header ~pp_h

let reporter ppf =
  let pid = Unix.getpid () in
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%6d]%a[%a]: " ^^ fmt ^^ "\n%!")
        pid pp_header (level, header)
        Fmt.(styled `Magenta (fmt "%10s"))
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer ();
  Logs.set_level level;
  let reporter = reporter Fmt.stderr in
  Logs.set_reporter reporter;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let inet_addr =
  let parser str =
    match Unix.inet_addr_of_string str with
    | inet_addr -> Ok inet_addr
    | exception _ -> Error (msgf "Invalid address: %S" str)
  in
  let pp = Fmt.using Unix.string_of_inet_addr Fmt.string in
  Arg.conv ~docv:"<inet-addr>" (parser, pp)

let addr_inet ~default =
  let parser str =
    match Ipaddr.with_port_of_string ~default str with
    | Ok (ipaddr, port) ->
        Ok (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
    | Error _ as err -> err
  in
  let pp ppf = function
    | Unix.ADDR_INET (inet_addr, port) ->
        Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
    | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  in
  Arg.conv ~docv:"<addr>" (parser, pp)

let addr ~default =
  let parser str =
    match Ipaddr.with_port_of_string ~default str with
    | Ok (ipaddr, port) -> Ok (`Inet (Ipaddr_unix.to_inet_addr ipaddr, port))
    | Error _ -> (
        let ( >>= ) = Result.bind in
        match String.split_on_char ':' str with
        | [ domain_name ] ->
            Domain_name.of_string domain_name >>= Domain_name.host
            >>= fun domain_name -> Ok (`Domain (domain_name, default))
        | domain_name :: port -> (
            let port = String.concat ":" port in
            try
              Domain_name.of_string domain_name >>= Domain_name.host
              >>= fun domain_name ->
              Ok (`Domain (domain_name, int_of_string port))
            with _ -> Error (`Msg "Invalid port"))
        | _ -> assert false)
  in
  let pp ppf = function
    | `Inet (inet_addr, port) ->
        Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
    | `Domain (domain_name, port) ->
        Fmt.pf ppf "%a:%d" Domain_name.pp domain_name port
  in
  Arg.conv ~docv:"<addr>" (parser, pp)

let string_to_int_array str =
  let res = Array.make (String.length str / 2) 0 in
  for i = 0 to (String.length str / 2) - 1 do
    res.(i) <- (Char.code str.[i * 2] lsl 8) lor Char.code str.[(i * 2) + 1]
  done;
  res

let int_array_to_string arr =
  let buf = Bytes.create (Array.length arr * 2) in
  for i = 0 to Array.length arr - 1 do
    Bytes.set buf (2 * i) (Char.unsafe_chr (arr.(i) lsr 8));
    Bytes.set buf ((2 * i) + 1) (Char.unsafe_chr arr.(i))
  done;
  Bytes.unsafe_to_string buf

let seed =
  let parser str =
    match Base64.decode str with
    | Ok seed -> Ok (string_to_int_array seed)
    | Error _ as err -> err
  in
  let pp = Fmt.using (Base64.encode_exn <.> int_array_to_string) Fmt.string in
  Arg.conv ~docv:"<seed>" (parser, pp)

let seed =
  let doc =
    "The seed (in base64) used to initialize the random number generator."
  in
  Arg.(value & opt (some seed) None & info [ "s"; "seed" ] ~doc ~docv:"<seed>")

let setup_random = function
  | None -> Random.State.make_self_init ()
  | Some seed -> Random.State.make seed

let setup_random = Term.(const setup_random $ seed)

let secure_port =
  let doc = "The port of the relay where secured rooms are available." in
  Arg.(value & opt int 9001 & info [ "secure-port" ] ~doc ~docv:"<port>")

let setup_password quiet g password =
  match password with
  | Some password -> password
  | None ->
      let t = Password.compile Dict.En.words in
      let ps =
        Array.init 2 (fun _ ->
            let len = Random.State.int g 5 in
            Password.generate ~g t len)
      in
      let password = Fmt.str "%s-%s" ps.(0) ps.(1) in
      if not quiet then Fmt.pr "Password: %s\n%!" password;
      password

let setup_password password =
  Term.(const setup_password $ setup_logs $ setup_random $ password)

let setup_temp = Option.iter Temp.set_default_directory

let temp =
  let doc = "The temporary directory used to generate the archive." in
  let env = Cmd.Env.info "BOB_TMP" in
  let directory =
    let parser str =
      match Fpath.of_string str with
      | Ok v ->
          if not (Sys.file_exists str) then
            Error (msgf "%a does not exist" Fpath.pp v)
          else if not (Sys.is_directory str) then
            Error (msgf "%a is not a directory" Fpath.pp v)
          else Ok (Fpath.to_dir_path v)
      | Error _ as err -> err
    in
    Arg.conv (parser, Fpath.pp)
  in
  Arg.(
    value
    & opt (some directory) None
    & info [ "temp" ] ~docs:common_options ~doc ~docv:"<directory>" ~env)

let setup_temp = Term.(const setup_temp $ temp)

let nameserver_of_string str =
  let ( let* ) = Result.bind in
  match String.split_on_char ':' str with
  | "tls" :: rest -> (
      let str = String.concat ":" rest in
      match String.split_on_char '!' str with
      | [ nameserver ] ->
          let* ipaddr, port =
            Ipaddr.with_port_of_string ~default:853 nameserver
          in
          let* authenticator = Ca_certs.authenticator () in
          let tls = Tls.Config.client ~authenticator () in
          Ok (`Tls (tls, ipaddr, port))
      | nameserver :: authenticator ->
          let* ipaddr, port =
            Ipaddr.with_port_of_string ~default:853 nameserver
          in
          let authenticator = String.concat "!" authenticator in
          let* authenticator = X509.Authenticator.of_string authenticator in
          let time () = Some (Ptime.v (Ptime_clock.now_d_ps ())) in
          let authenticator = authenticator time in
          let tls = Tls.Config.client ~authenticator () in
          Ok (`Tls (tls, ipaddr, port))
      | [] -> assert false)
  | "tcp" :: nameserver | nameserver ->
      let str = String.concat ":" nameserver in
      let* ipaddr, port = Ipaddr.with_port_of_string ~default:53 str in
      Ok (`Plaintext (ipaddr, port))

let nameserver =
  let parser = nameserver_of_string in
  let pp ppf = function
    | `Tls (_, ipaddr, 853) ->
        Fmt.pf ppf "tls:%a!<authenticator>" Ipaddr.pp ipaddr
    | `Tls (_, ipaddr, port) ->
        Fmt.pf ppf "tls:%a:%d!<authenticator>" Ipaddr.pp ipaddr port
    | `Plaintext (ipaddr, 53) -> Fmt.pf ppf "%a" Ipaddr.pp ipaddr
    | `Plaintext (ipaddr, port) -> Fmt.pf ppf "%a:%d" Ipaddr.pp ipaddr port
  in
  Arg.conv (parser, pp) ~docv:"<nameserver>"

let nameservers =
  let doc = "The nameserver used to resolve domain-names." in
  let env = Cmd.Env.info "BOB_NAMESERVER" in
  Arg.(
    value
    & opt_all nameserver [ `Plaintext (Ipaddr.of_string "8.8.8.8", 53) ]
    & info [ "nameserver" ] ~docs:common_options ~doc ~docv:"<nameserver>" ~env)

let setup_dns nameservers =
  Bob_dns.create ~edns:`Auto ~nameservers:(`Tcp, nameservers) ()

let setup_dns = Term.(const setup_dns $ nameservers)
