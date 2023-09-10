open Stdbob
open Socks

let fully_write fd bs =
  let open Fiber in
  let rec go (off, len) =
    if len > 0 then
      Fiber.write fd bs ~off ~len >>= function
      | Ok len' -> go (off + len', len - len')
      | Error err -> Fiber.return (Error err)
    else Fiber.return (Ok ())
  in
  go (0, Bigarray.Array1.dim bs)

let addr_to_string_and_int = function
  | `Inet (inet_addr, port) -> (Unix.string_of_inet_addr inet_addr, port)
  | `Domain (domain_name, port) -> (Domain_name.to_string domain_name, port)

type addr =
  [ `Inet of Unix.inet_addr * int | `Domain of [ `host ] Domain_name.t * int ]

type identifier = Username of string | Credential of string * string | None
type server = [ `Socks4a | `Socks5 ] * identifier * addr

let parser str =
  let default = 1080 in
  let protocol, str =
    match cuts ~sep:"://" str with
    | [ str ] -> (`Socks4a, str)
    | ("socks4" | "socks4a") :: str -> (`Socks4a, String.concat "://" str)
    | "socks5" :: str -> (`Socks5, String.concat "://" str)
    | protocol :: _ -> Fmt.invalid_arg "Invalid protocol: %S" protocol
    | [] -> assert false
  in
  let credential, str =
    match (protocol, String.split_on_char '@' str) with
    | _, [] -> assert false
    | _, [ str ] -> (None, str)
    | `Socks4a, username :: str -> (Username username, String.concat "@" str)
    | `Socks5, value :: str ->
        let[@warning "-8"] (username :: password) =
          String.split_on_char ':' value
        in
        ( Credential (username, String.concat ":" password),
          String.concat "@" str )
  in
  match Ipaddr.with_port_of_string ~default str with
  | Ok (ipaddr, port) ->
      Ok (protocol, credential, `Inet (Ipaddr_unix.to_inet_addr ipaddr, port))
  | Error _ -> (
      let ( >>= ) = Result.bind in
      match String.split_on_char ':' str with
      | [ domain_name ] ->
          Domain_name.of_string domain_name >>= Domain_name.host
          >>= fun domain_name ->
          Ok (protocol, credential, `Domain (domain_name, default))
      | domain_name :: port -> (
          let port = String.concat ":" port in
          try
            Domain_name.of_string domain_name >>= Domain_name.host
            >>= fun domain_name ->
            Ok (protocol, credential, `Domain (domain_name, int_of_string port))
          with _ -> Error (`Msg "Invalid port"))
      | _ -> assert false)

let parser str = try parser str with Invalid_argument err -> Error (`Msg err)

let pp_addr ppf = function
  | `Inet (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | `Domain (domain_name, port) ->
      Fmt.pf ppf "%a:%d" Domain_name.pp domain_name port

let pp ppf (protocol, credential, addr) =
  match (protocol, credential, addr) with
  | `Socks5, Credential (username, password), addr ->
      Fmt.pf ppf "socks5://%s:%s@%a" username password pp_addr addr
  | `Socks5, None, addr -> Fmt.pf ppf "socks5://%a" pp_addr addr
  | `Socks4a, None, addr -> Fmt.pf ppf "socks4a://%a" pp_addr addr
  | `Socks4a, Username username, addr ->
      Fmt.pf ppf "socks4a://%s@%a" username pp_addr addr
  | _ -> assert false

let open_write_error = function
  | Ok _ as value -> value
  | Error `Closed -> Error `Closed
  | Error (`Unix _ as err) -> Error err

let socks5_struct_to_sockaddr { Socks.port; address } =
  let addr =
    match address with
    | IPv4_address ipv4 -> Ipaddr.V4 ipv4
    | IPv6_address ipv6 -> Ipaddr.V6 ipv6
    | Domain_address _ ->
        Fmt.invalid_arg "Unexpected domain to cast to a sockaddr"
  in
  Unix.ADDR_INET (Ipaddr_unix.to_inet_addr addr, port)

let pp_socks5_reply ppf = function
  | Socks.Succeeded -> Fmt.string ppf "Succeeeded"
  | Socks.General_socks_server_failure ->
      Fmt.string ppf "General socks server failure"
  | Socks.Connection_not_allowed_by_ruleset ->
      Fmt.string ppf "Connection not allowed by ruleset"
  | Socks.Network_unreachable -> Fmt.string ppf "Network unreachable"
  | Socks.Host_unreachable -> Fmt.string ppf "Host unreachable"
  | Socks.Connection_refused -> Fmt.string ppf "Connection refused"
  | Socks.TTL_expired -> Fmt.string ppf "TTL expired"
  | Socks.Command_not_supported -> Fmt.string ppf "Command not supported"
  | Socks.Address_type_not_supported ->
      Fmt.string ppf "Address type not supported"
  | Socks.Unassigned -> Fmt.string ppf "Unassigned"

let connect ~happy_eyeballs ~server dst =
  let open Fiber in
  let hostname, port = addr_to_string_and_int dst in
  match server with
  | `Socks4a, credential, addr ->
      Bob_happy_eyeballs.connect happy_eyeballs addr
      >>? fun (sockaddr, socket) ->
      let username =
        match credential with
        | Username username -> username
        | None -> ""
        | Credential _ -> assert false
      in
      let[@warning "-8"] (Ok req) =
        Socks.make_socks4_request ~username ~hostname port
      in
      fully_write socket
        (bigstring_of_string req ~off:0 ~len:(String.length req))
      >>| open_write_error
      >>? fun () ->
      let rec go payload =
        match Socks.parse_socks4_response payload with
        | Ok "" -> Fiber.return (Ok (sockaddr, socket))
        | Ok _ ->
            (* XXX(dinosaure): we know how we should talk to the relay and,
               actually, the server waits for an identifier from us, it should
               not send something at this stage. *)
            Fiber.return (Error (`Msg "Unexpected leftover data"))
        | Error Socks.Rejected ->
            Fiber.return (Error (`Msg "Rejected by the SOCKSv4a server"))
        | Error Socks.Incomplete_response -> (
            Fiber.read socket >>| reword_error (fun err -> `Unix err)
            >>? function
            | `End ->
                Fiber.return
                  (Error (`Msg "Incomplete response from the SOCKSv4a server"))
            | `Data bs -> go (payload ^ bigstring_to_string bs))
      in
      go ""
  | `Socks5, credential, addr ->
      Bob_happy_eyeballs.connect happy_eyeballs addr
      >>? fun (_sockaddr, socket) ->
      let socks5_request =
        match dst with
        | `Domain (domain_name, port) ->
            let domain_name = Domain_name.to_string domain_name in
            Socks.Connect
              { Socks.port; address = Socks.Domain_address domain_name }
        | `Inet (inet_addr, port) -> (
            match Ipaddr_unix.of_inet_addr inet_addr with
            | Ipaddr.V4 addr ->
                Socks.Connect { Socks.port; address = Socks.IPv4_address addr }
            | Ipaddr.V6 addr ->
                Socks.Connect { Socks.port; address = Socks.IPv6_address addr })
      in
      let auth_req =
        match credential with
        | None -> Socks.make_socks5_auth_request ~username_password:false
        | Credential _ -> Socks.make_socks5_auth_request ~username_password:true
        | Username _ -> assert false
      in
      let[@warning "-8"] (Ok req) = Socks.make_socks5_request socks5_request in
      fully_write socket
        (bigstring_of_string auth_req ~off:0 ~len:(String.length auth_req))
      >>| open_write_error
      >>? fun () ->
      let rec go payload =
        match Socks.parse_socks5_username_password_response payload with
        | Ok (No_acceptable_methods, _) ->
            Fiber.return
              (Error
                 (`Msg
                   "No acceptable authentication methods for the SOCKSv5 server"))
        | Ok (No_authentication_required, leftover) when credential = None ->
            Fiber.return (Ok leftover)
        | Ok (_, _) ->
            Fiber.return
              (Error (`Msg "Unexpected response from the SOCKSv5 server"))
        | Error `Invalid_request ->
            Fiber.return
              (Error (`Msg "Invalid response from the SOCKSv5 server"))
        | Error `Incomplete_request -> (
            Fiber.read socket >>| reword_error (fun err -> `Unix err)
            >>? function
            | `End ->
                Fiber.return
                  (Error (`Msg "Incomplete response from the SOCKSv5 server"))
            | `Data bs -> go (payload ^ bigstring_to_string bs))
      in
      go "" >>? fun payload ->
      fully_write socket
        (bigstring_of_string req ~off:0 ~len:(String.length req))
      >>| open_write_error
      >>? fun () ->
      let rec go payload =
        match Socks.parse_socks5_response payload with
        | Ok (Socks.Succeeded, value, "") ->
            let sockaddr = socks5_struct_to_sockaddr value in
            Fiber.return (Ok (sockaddr, socket))
        | Ok (reply, _, leftover) ->
            Fiber.return
              (Error
                 (msgf "Error from the SOCKSv5 server: %a (%S)" pp_socks5_reply
                    reply leftover))
        | Error Socks.Invalid_response ->
            Fiber.return (Error (`Msg "Rejected by the SOCKSv5 server"))
        | Error Socks.Incomplete_response -> (
            Fiber.read socket >>| reword_error (fun err -> `Unix err)
            >>? function
            | `End ->
                Fiber.return
                  (Error (`Msg "Incomplete response from the SOCKSv5 server"))
            | `Data bs -> go (payload ^ bigstring_to_string bs))
      in
      go payload
