open Stdbob

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
    | Ok len' ->
        if len' - len <= 0 then Fiber.return (Ok ())
        else go fd bstr (off + len') (len - len')
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

let rec run_protocol secure_flow value =
  let open Fiber in
  match value with
  | Bob_protocol.Protocol.Return value -> Fiber.return (Ok value)
  | Bob_protocol.Protocol.Error err -> Fiber.return (Error err)
  | Bob_protocol.Protocol.Write { buffer; off; len; continue } ->
      Logs.debug (fun m ->
          m "~> @[<hov>%a@]"
            (Hxd_string.pp Hxd.default)
            (String.sub buffer off len));
      let { Cstruct.buffer; off; len } = Cstruct.of_string buffer ~off ~len in
      Crypto.send secure_flow buffer ~off ~len
      >>| Result.map_error (fun err -> `Read err)
      >>? fun len -> run_protocol secure_flow (continue len)
  | Bob_protocol.Protocol.Read { buffer; off; len = _; continue } -> (
      Crypto.recv secure_flow >>| Result.map_error (fun err -> `Write err)
      >>? function
      | `End -> run_protocol secure_flow (continue `End)
      | `Data bstr ->
          let len = Bigarray.Array1.dim bstr in
          bigstring_blit_to_bytes bstr ~src_off:0 buffer ~dst_off:off ~len;
          Logs.debug (fun m ->
              m "<~ @[<hov>%a@]"
                (Hxd_string.pp Hxd.default)
                (Bytes.sub_string buffer off len));
          run_protocol secure_flow (continue (`Len len)))

let sockaddr_with_secure_port sockaddr secure_port =
  match sockaddr with
  | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, secure_port)
  | Unix.ADDR_UNIX _ -> invalid_arg "Invalid sockaddr"

let on_error on_error k =
  let open Fiber in
  k () >>= function
  | Error err -> on_error err >>= fun () -> Fiber.return (Error err)
  | Ok _ as value -> Fiber.return value

type error =
  [ `Msg of string
  | `Handshake of Bob_clear.error
  | `Write of Crypto.error
  | `Read of Crypto.write_error
  | `Connect of Clear.error
  | Bob_protocol.Protocol.error
  | Bob_protocol.Machine.error
  | Bob_unix.error ]

let run person g he addr secure_port password reproduce =
  let open Fiber in
  Bob_happy_eyeballs.connect he addr >>? fun (sockaddr, socket) ->
  (match person with
  | `Bob ->
      let secret, _ = Spoke.generate ~g ~password ~algorithm:Spoke.Pbkdf2 16 in
      Bob_clear.server socket ~reproduce ~g secret
  | `Alice ->
      Bob_clear.client socket ~reproduce
        ~choose:(Fun.const (Fiber.return `Accept))
        ~g password)
  >>| Result.map_error (fun err -> `Handshake err)
  >>? fun (identity, ciphers, shared_keys) ->
  Fmt.pr ">>> Found a peer: %S\n%!" identity;
  let sockaddr = sockaddr_with_secure_port sockaddr secure_port in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let { Unix.p_proto; _ } =
    try Unix.getprotobyname "tcp"
    with _ ->
      (* fail on Windows *) { p_name = "tcp"; p_aliases = [||]; p_proto = 0 }
  in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM p_proto in
  Fiber.connect socket sockaddr >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  on_error (fun _err -> Fiber.close socket) @@ fun () ->
  Bob_unix.init_peer socket ~identity
  >>| Result.map_error (fun err -> (err :> error))
  >>? fun () ->
  let ciphers, shared_keys =
    match person with
    | `Bob -> (ciphers, shared_keys)
    | `Alice -> (flip ciphers, flip shared_keys)
  in
  let secure_flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
  let state = Bob_protocol.Protocol.state () in
  let monad =
    match person with
    | `Bob ->
        let metadata = Bob_protocol.Metadata.v1 ~size:1L `File in
        Bob_protocol.Machine.bob ~metadata state
    | `Alice -> Bob_protocol.Machine.alice state
  in
  run_protocol secure_flow monad
  >>| Result.map_error (fun err -> (err :> error))
  >>? function
  | `Ready_to_send -> Fiber.close socket >>= fun () -> Fiber.return (Ok ())
  | `Ready_to_resume _resume ->
      Fiber.close socket >>= fun () -> Fiber.return (Ok ())
  | `Ready_to_recv _metadata ->
      Fiber.close socket >>= fun () -> Fiber.return (Ok ())
  | `Quit -> Fiber.return (Error (`Read `Closed))

let pp_error ppf = function
  | `Msg err -> Fmt.pf ppf "%s" err
  | `Handshake err -> Bob_clear.pp_error ppf err
  | #Bob_protocol.Protocol.error as err ->
      Bob_protocol.Protocol.pp_error ppf err
  | `Write err -> Crypto.pp_error ppf err
  | `Read err -> Crypto.pp_write_error ppf err
  | `Connect err -> Clear.pp_error ppf err
  | #Bob_unix.error as err -> Bob_unix.pp_error ppf err
  | #Bob_protocol.Machine.error as err -> Bob_protocol.Machine.pp_error ppf err

let run _quiet g () (_, he) addr secure_port password person reproduce =
  match Fiber.run (run person g he addr secure_port password reproduce) with
  | Ok () -> `Ok 0
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.argv.(0) pp_error err;
      `Ok 1

open Cmdliner
open Args

let password =
  let doc = "The password to share." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"<password>")

let person =
  let flags = [ (`Alice, Arg.info [ "alice" ]); (`Bob, Arg.info [ "bob" ]) ] in
  Arg.(value & vflag `Alice flags)

let term =
  Term.(
    ret
      (const run $ term_setup_logs $ term_setup_random $ term_setup_temp
     $ term_setup_dns $ relay $ secure_port $ password $ person $ reproduce))

let cmd =
  let doc =
    "A simple program to talk with another person via a secured canal."
  in
  let man = [ `S Manpage.s_description; `P "" ] in
  Cmd.v (Cmd.info "talk" ~doc ~man) term

let () = exit @@ Cmd.eval' cmd
