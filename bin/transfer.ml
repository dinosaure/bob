open Stdbob

let src = Logs.Src.create "bob.transfer"

module Log = (val Logs.src_log src : Logs.LOG)

let addr_with_secure_port addr secure_port =
  match addr with
  | `Inet (inet_addr, _) -> `Inet (inet_addr, secure_port)
  | `Domain (host, _) -> `Domain (host, secure_port)

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
  | `Connect of [ `Closed | `Msg of string | `Unix of Unix.error ]
  | `Blocking_connect of Connect.error
  | `Crypto of [ Crypto.write_error | Crypto.error ] ]

let pp_error ppf = function
  | `Blocking_connect err -> Connect.pp_error ppf err
  | `Connect `Closed -> Fmt.pf ppf "connect(): Connection reset by peer"
  | `Connect (`Msg msg) -> Fmt.pf ppf "connect(): %s" msg
  | `Connect (`Unix errno) ->
      Fmt.pf ppf "connect(): %s" (Unix.error_message errno)
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
    ~happy_eyeballs ?through addr stream =
  let open Fiber in
  (match through with
  | Some server -> Bob_socks.connect ~happy_eyeballs ~server addr
  | None -> Bob_happy_eyeballs.connect happy_eyeballs addr)
  >>| reword_error (fun err -> `Connect err)
  >>? fun (_sockaddr, socket) ->
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
  let stop _flow =
    (try finalise () with _ -> ());
    (* TODO(dinosaure): should we close? *)
    Fiber.return ()
  in
  Stream.Source { init; pull; stop }

let receive ?(reporter = Fiber.ignore) ?(finalise = ignore) ~identity ~ciphers
    ~shared_keys ~happy_eyeballs ?through addr =
  let open Fiber in
  (match through with
  | Some server -> Bob_socks.connect ~happy_eyeballs ~server addr
  | None -> Bob_happy_eyeballs.connect happy_eyeballs addr)
  >>| reword_error (fun err -> `Connect err)
  >>? fun (_sockaddr, socket) ->
  Bob_unix.init_peer socket ~identity >>= function
  | Error (#Bob_unix.error as err) ->
      Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
  | Ok () ->
      Fiber.return
        (Ok (crypto_of_flow ~reporter ~finalise ~ciphers ~shared_keys socket))
