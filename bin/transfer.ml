open Stdbob

let src = Logs.Src.create "bob.transfer"

module Log = (val Logs.src_log src : Logs.LOG)

let sockaddr_with_secure_port sockaddr secure_port =
  match sockaddr with
  | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, secure_port)
  | Unix.ADDR_UNIX _ -> invalid_arg "Invalid sockaddr"

let max_packet =
  2 + Bob_unix.Crypto.max_packet + 16 (* header + data + tag_size *)

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
    Fiber.read ~len:(max_packet * 2) fd >>= function
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

type error =
  [ Bob_unix.error
  | `Connect of Unix.error
  | `Blocking_connect of Connect.error
  | `Crypto of [ Crypto.write_error | Crypto.error ] ]

let pp_error ppf = function
  | `Blocking_connect err -> Connect.pp_error ppf err
  | `Connect errno -> Fmt.pf ppf "connect(): %s" (Unix.error_message errno)
  | `Crypto (#Crypto.write_error as err) -> Crypto.pp_write_error ppf err
  | `Crypto (#Crypto.error as err) -> Crypto.pp_error ppf err
  | #Bob_unix.error as err -> Bob_unix.pp_error ppf err

let open_error = function Ok _ as v -> v | Error #error as v -> v

type crypto_error = [ Crypto.error | Crypto.write_error ]

let crypto_of_flow ~reporter ~ciphers ~shared_keys socket =
  let open Fiber in
  let init () =
    let flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
    Fiber.return (Ok flow)
  in
  let push state bstr =
    match state with
    | Error err -> Fiber.return (Error (err :> crypto_error))
    | Ok flow -> (
        Crypto.send flow bstr ~off:0 ~len:(Bigarray.Array1.dim bstr)
        >>= function
        | Ok len -> reporter len >>= fun () -> Fiber.return (Ok flow)
        | Error err ->
            Crypto.close flow >>= fun () ->
            Fiber.return (Error (err :> crypto_error)))
  in
  let full = Fiber.always false in
  let stop = function
    | Error err -> Fiber.return (Error (err :> crypto_error))
    | Ok flow -> (
        Crypto.recv flow >>= function
        | Ok `End -> Fiber.return (Ok ())
        | Ok (`Data bstr) ->
            Log.err (fun m -> m "Received unexpected payload:");
            Log.err (fun m ->
                m "@[<hov>%a@]"
                  (Hxd_string.pp Hxd.default)
                  (bigstring_to_string bstr));
            Fiber.return (Error `Corrupted)
        | Error err -> Fiber.return (Error (err :> crypto_error)))
  in
  Stream.Sink { init; push; full; stop }

let transfer ?chunk:_ ?(reporter = Fiber.ignore) ~identity ~ciphers ~shared_keys
    sockaddr stream =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Fiber.connect socket sockaddr >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  Bob_unix.init_peer socket ~identity >>= function
  | Error (#Bob_unix.error as err) ->
      Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
  | Ok () ->
      let open Stream in
      let crypto = crypto_of_flow ~reporter ~ciphers ~shared_keys socket in
      Stream.into crypto stream
      >>| reword_error (fun err ->
              `Crypto (err :> [ Crypto.write_error | Crypto.error ]))

let crypto_of_flow ~reporter ~finalise ~ciphers ~shared_keys socket =
  let open Fiber in
  let init () =
    let flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
    Fiber.return flow
  in
  let pull flow =
    Crypto.recv flow >>= function
    | Ok (`Data bstr) ->
        reporter (Bigarray.Array1.dim bstr) >>= fun () ->
        Fiber.return (Some (bstr, flow))
    | Ok `End ->
        finalise ();
        Fiber.return None
    | Error _ -> Crypto.close flow >>= fun () -> Fiber.return None
  in
  let stop flow =
    (try finalise () with _ -> ());
    Crypto.close flow
  in
  Stream.Source { init; pull; stop }

let receive ?(reporter = Fiber.ignore) ?(finalise = ignore) ~identity ~ciphers
    ~shared_keys sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Fiber.connect socket sockaddr >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  Bob_unix.init_peer socket ~identity >>= function
  | Error (#Bob_unix.error as err) ->
      Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
  | Ok () ->
      Fiber.return
        (Ok (crypto_of_flow ~reporter ~finalise ~ciphers ~shared_keys socket))
