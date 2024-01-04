open Stdbob

type value = Value : 'a Bob_protocol.Protocol.packet * 'a -> value

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

(* XXX(dinosaure): copy-pasta with [transfer.ml], TODO! *)
module Crypto = Bob_unix.Crypto.Make (struct
  include Bob_unix.Fiber

  type flow = Unix.file_descr
  type error = Unix.error
  type write_error = [ `Closed | `Unix of Unix.error ]

  let pp_error ppf errno = Fmt.string ppf (Unix.error_message errno)

  let pp_write_error ppf = function
    | `Closed -> Fmt.string ppf "Connection closed by peer"
    | `Unix errno -> Fmt.string ppf (Unix.error_message errno)

  let read fd =
    Fiber.read ~len:(4 * 0xFFFF) fd >>= function
    | Error _ as err -> Fiber.return err
    | Ok `End -> Fiber.return (Ok `Eof)
    | Ok (`Data bstr) -> Fiber.return (Ok (`Data (Cstruct.of_bigarray bstr)))

  let rec write fd cs =
    let { Cstruct.buffer; off; len } = cs in
    go fd buffer off len

  and go fd bstr off len =
    Fiber.write fd bstr ~off ~len >>= function
    | Error _ as err -> Fiber.return err
    | Ok len' when len' = len -> Fiber.return (Ok ())
    | Ok len' -> go fd bstr (off + len') (len - len')
end)

module Clear = struct
  type fd = Unix.file_descr
  type error = Unix.error
  type write_error = [ `Closed | `Unix of Unix.error ]

  let pp_error ppf errno = Fmt.pf ppf "%s" (Unix.error_message errno)

  let pp_write_error ppf = function
    | `Closed -> Fmt.pf ppf "Connection closed by peer"
    | `Unix errno -> Fmt.pf ppf "%s" (Unix.error_message errno)

  let of_file_descr fd = Fiber.return (Ok fd)
  let recv = Fiber.read ?len:None
  let send = Fiber.write
  let close = Fiber.close
end

module Bob_clear = struct
  include Bob_unix.Make (Clear)
end

let rec talk secure_flow value =
  let open Fiber in
  match value with
  | Bob_protocol.State.Return value -> Fiber.return (Ok value)
  | Bob_protocol.State.Error err -> Fiber.return (Error err)
  | Bob_protocol.State.Write { buffer; off; len; continue } ->
      let { Cstruct.buffer; off; len } = Cstruct.of_string buffer ~off ~len in
      Crypto.send secure_flow buffer ~off ~len
      >>| Result.map_error (fun err -> `Read err)
      >>? fun len -> talk secure_flow (continue len)
  | Bob_protocol.State.Read { buffer; off; len = _; continue } -> (
      Crypto.recv secure_flow >>| Result.map_error (fun err -> `Write err)
      >>? function
      | `End -> talk secure_flow (continue `End)
      | `Data bstr ->
          let len = Bigarray.Array1.dim bstr in
          bigstring_blit_to_bytes bstr ~src_off:0 buffer ~dst_off:off ~len;
          talk secure_flow (continue (`Len len)))

let send stdin state secure_flow =
  let open Fiber in
  getline stdin >>= function
  | None ->
      talk secure_flow
        (Bob_protocol.Protocol.encode state Bob_protocol.Protocol.quit ())
      >>? fun () -> Fiber.return (Ok `Quit)
  | Some cmd -> (
      let v =
        match cmd with
        | "ping" -> Ok (Value (Bob_protocol.Protocol.ping, ()))
        | "pong" -> Ok (Value (Bob_protocol.Protocol.pong, ()))
        | cmd -> error_msgf "Invalid command: %S" cmd
      in
      match v with
      | Ok (Value (packet, value)) ->
          talk secure_flow (Bob_protocol.Protocol.encode state packet value)
          >>? fun () -> Fiber.return (Ok `Sent)
      | Error _ as err -> Fiber.return err)

let recv state secure_flow =
  let open Fiber in
  talk secure_flow
    (Bob_protocol.Protocol.decode state Bob_protocol.Protocol.any)
  >>? fun (Any (packet, value)) ->
  Fiber.return (Ok (`Received (Value (packet, value))))

let rec pp_value ppf (Value (packet, value)) =
  match packet with
  | Bob_protocol.Protocol.Any ->
      let (Any (packet, value)) = value in
      pp_value ppf (Value (packet, value))
  | Ping | Pong | Quit -> Bob_protocol.Protocol.pp ppf packet

let rec repl stdin state secure_flow =
  let open Fiber in
  pick
    (fun () -> send stdin state secure_flow)
    (fun () -> recv state secure_flow)
  >>? function
  | `Quit -> Fiber.return (Ok ())
  | `Sent ->
      Fmt.pr "# %!";
      repl stdin state secure_flow
  | `Received value ->
      Fmt.pr "\n~> %a\n%!# %!" pp_value value;
      repl stdin state secure_flow

let sockaddr_with_secure_port sockaddr secure_port =
  match sockaddr with
  | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, secure_port)
  | Unix.ADDR_UNIX _ -> invalid_arg "Invalid sockaddr"

let run g he addr secure_port password reproduce =
  let open Fiber in
  Bob_happy_eyeballs.connect he addr >>? fun (sockaddr, socket) ->
  Bob_clear.client socket ~reproduce
    ~choose:(Fun.const (Fiber.return `Accept))
    ~g password
  >>| Result.map_error (fun err -> `Handshake err)
  >>? fun (_identity, ciphers, shared_keys) ->
  let sockaddr = sockaddr_with_secure_port sockaddr secure_port in
  let domain = Unix.domain_of_sockaddr sockaddr in
  Fiber.connect socket sockaddr >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  let { Unix.p_proto; _ } =
    try Unix.getprotobyname "tcp"
    with _ ->
      (* fail on Windows *) { p_name = "tcp"; p_aliases = [||]; p_proto = 0 }
  in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM p_proto in
  let secure_flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
  let state = Bob_protocol.Protocol.state () in
  repl Unix.stdin state secure_flow >>= fun res ->
  Fiber.close socket >>= fun () -> Fiber.return res

let pp_error ppf = function
  | `Msg err -> Fmt.pf ppf "%s" err
  | `Handshake err -> Bob_clear.pp_error ppf err
  | #Bob_protocol.Protocol.error as err ->
      Bob_protocol.Protocol.pp_error ppf err
  | `Write err -> Crypto.pp_error ppf err
  | `Read err -> Crypto.pp_write_error ppf err
  | `Connect err -> Clear.pp_error ppf err

let run _quiet g () (_, he) addr secure_port password reproduce =
  match Fiber.run (run g he addr secure_port password reproduce) with
  | Ok () -> `Ok 0
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.argv.(0) pp_error err;
      `Ok 1

open Cmdliner
open Args

let password =
  let doc = "The password to share." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"<password>")

let term =
  Term.(
    ret
      (const run $ term_setup_logs $ term_setup_random $ term_setup_temp
     $ term_setup_dns $ relay $ secure_port $ password $ reproduce))

let _cmd =
  let doc =
    "A simple program to talk with another person via a secured canal."
  in
  let man = [ `S Manpage.s_description; `P "" ] in
  Cmd.v (Cmd.info "talk" ~doc ~man) term
