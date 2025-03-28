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
        ("[%6d]%a[%a]: @[<hov>" ^^ fmt ^^ "@]\n%!")
        pid pp_header (level, header)
        Fmt.(styled `Magenta (fmt "%20s"))
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

let term_setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

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

let default_relay =
  let osau_re = Domain_name.(host_exn (of_string_exn "osau.re")) in
  `Domain (osau_re, 9000)

let relay =
  let doc = "The address of the relay." in
  Arg.(
    value
    & opt (addr ~default:9000) default_relay
    & info [ "r"; "relay" ] ~doc ~docv:"<addr>:<port>")

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

let term_setup_random = Term.(const setup_random $ seed)

let secure_port =
  let doc = "The port of the relay where secured rooms are available." in
  Arg.(value & opt int 9001 & info [ "secure-port" ] ~doc ~docv:"<port>")

let setup_password quiet g password =
  match password with
  | Some password -> (quiet, g, password)
  | None ->
      let t = Password.compile Dict.En.words in
      let ps =
        Array.init 2 (fun _ ->
            let len = Random.State.int g 5 in
            Password.generate ~g t len)
      in
      let password = Fmt.str "%s-%s" ps.(0) ps.(1) in
      if not quiet then Fmt.pr "Password: %s\n%!" password;
      (quiet, g, password)

let term_setup_password password =
  Term.(const setup_password $ term_setup_logs $ term_setup_random $ password)

let setup_temp = Option.iter Temp.set_default_directory

let temp =
  let doc = "The temporary directory used to generate the archive." in
  let env = Cmd.Env.info "BOB_TMP" in
  let directory =
    let parser str =
      match Bob_fpath.of_string str with
      | Ok v ->
          if not (Sys.file_exists str) then
            Error (msgf "%a does not exist" Bob_fpath.pp v)
          else if not (Sys.is_directory str) then
            Error (msgf "%a is not a directory" Bob_fpath.pp v)
          else Ok (Bob_fpath.to_dir_path v)
      | Error _ as err -> err
    in
    Arg.conv (parser, Bob_fpath.pp)
  in
  Arg.(
    value
    & opt (some directory) None
    & info [ "temp" ] ~docs:common_options ~doc ~docv:"<directory>" ~env)

let term_setup_temp = Term.(const setup_temp $ temp)
let setup_happy_eyeballs () = Bob_happy_eyeballs.create ()
let term_setup_happy_eyeballs = Term.(const setup_happy_eyeballs $ const ())

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
          let tls = Result.get_ok tls in
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
          let tls = Result.get_ok tls in
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

let unicast_censurfridns_dk = Ipaddr.of_string_exn "89.233.43.71"

let unicast_censurfridns_dk =
  let time () = Some (Ptime.v (Ptime_clock.now_d_ps ())) in
  let authenticator =
    X509.Authenticator.of_string
      "key-fp:sha256:INSZEZpDoWKiavosV2/xVT8O83vk/RRwS+LTiL+IpHs="
    |> Result.get_ok
  in
  let cfg = Tls.Config.client ~authenticator:(authenticator time) () in
  let cfg = Result.get_ok cfg in
  `Tls (cfg, unicast_censurfridns_dk, 853)

let google_com = `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53)

let nameservers =
  let doc = "The nameserver used to resolve domain-names." in
  let env = Cmd.Env.info "BOB_NAMESERVER" in
  Arg.(
    value
    & opt_all nameserver [ unicast_censurfridns_dk; google_com ]
    & info [ "nameserver" ] ~docs:common_options ~doc ~docv:"<nameserver>" ~env)

let getaddrinfo dns =
  let getaddrinfo record domain_name =
    Bob_dns.getaddrinfo dns record domain_name
  in
  { Bob_happy_eyeballs.getaddrinfo }

let setup_dns nameservers happy_eyeballs =
  let dns =
    Bob_dns.create ~edns:`Auto ~nameservers:(`Tcp, nameservers) happy_eyeballs
  in
  Bob_happy_eyeballs.inject (getaddrinfo dns) happy_eyeballs;
  (dns, happy_eyeballs)

let term_setup_dns =
  Term.(const setup_dns $ nameservers $ term_setup_happy_eyeballs)

let compression =
  Arg.(
    value
    & vflag None
        [
          (Some true, info [ "compress" ] ~doc:"Explicitly compress objects.");
          ( Some false,
            info [ "no-compression" ]
              ~doc:
                "Explicitly store objects as they are (useful for video/image)."
          );
        ])

let yes =
  let doc = "Answer yes to all bob questions without prompting." in
  Arg.(value & flag & info [ "y"; "yes" ] ~doc)

let reproduce =
  let doc =
    "Reproduce the generation of packets. This option must only be used for \
     debugging."
  in
  Arg.(value & flag & info [ "reproduce" ] ~doc)

let destination =
  let doc =
    "Destination of the received document (file or folder). We accept only a \
     concrete destination ($(b,.) and $(b,..) are not allowed)."
  in
  let name =
    let parser = function
      | "." | ".." -> Error (`Msg "Invalid destination")
      | str -> (
          match Bob_fpath.of_string str with
          | Ok v when Sys.file_exists str ->
              Error (msgf "'%a' already exists" Bob_fpath.pp v)
          | Ok _ as v -> v
          | Error _ as error -> error)
    in
    let pp = Bob_fpath.pp in
    Arg.conv (parser, pp)
  in
  Arg.(value & opt (some name) None & info [ "o"; "output" ] ~doc ~docv:"<dst>")
