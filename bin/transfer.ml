let sockaddr_with_secure_port sockaddr secure_port =
  match sockaddr with
  | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, secure_port)
  | Unix.ADDR_UNIX _ -> invalid_arg "Invalid sockaddr"

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
    Fiber.read fd >>= function
    | Error _ as err -> Fiber.return err
    | Ok `End -> Fiber.return (Ok `Eof)
    | Ok (`Data str) -> Fiber.return (Ok (`Data (Cstruct.of_string str)))

  let write fd cs =
    let rec go str off len =
      Fiber.write fd ~off ~len str >>= function
      | Error _ as err -> Fiber.return err
      | Ok len' when len' = len -> Fiber.return (Ok ())
      | Ok len' -> go str (off + len') (len - len')
    in
    go (Cstruct.to_string cs) 0 (Cstruct.length cs)
end)

let ( >>? ) x f =
  Fiber.(x >>= function Ok x -> f x | Error err -> return (Error err))

let reword_error f = function Ok v -> Ok v | Error err -> Error (f err)

type error =
  [ Bob_unix.error
  | `Connect of Connect.error
  | `Crypto of [ Crypto.write_error | Crypto.error ] ]

let pp_error ppf = function
  | `Connect err -> Connect.pp_error ppf err
  | `Crypto (#Crypto.write_error as err) -> Crypto.pp_write_error ppf err
  | `Crypto (#Crypto.error as err) -> Crypto.pp_error ppf err
  | #Bob_unix.error as err -> Bob_unix.pp_error ppf err

let transfer ?(chunk = 0x1000) ?(reporter = ignore) ~identity ~ciphers
    ~shared_keys sockaddr fpath =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Fiber.return (Connect.connect socket sockaddr)
  >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  Bob_unix.init_peer socket ~identity >>= function
  | Error (#Bob_unix.error as err) ->
      Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
  | Ok () ->
      let ic = open_in_bin (Fpath.to_string fpath) in
      let flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
      let temp = Bytes.create chunk in
      let rec go () =
        match input ic temp 0 chunk with
        | 0 -> Crypto.close flow >>= fun () -> Fiber.return (Ok ())
        | len -> (
            Crypto.send flow (Bytes.unsafe_to_string temp) ~off:0 ~len
            >>= function
            | Ok _ ->
                reporter len;
                go ()
            | Error _ as err -> Crypto.close flow >>= fun () -> Fiber.return err
            )
      in
      go ()
      >>| reword_error (fun err ->
              `Crypto (err :> [ Crypto.write_error | Crypto.error ]))
      >>= fun res ->
      close_in ic;
      Fiber.return res

let save ?g ?(tmp : Pack.pattern = "pack-%s.pack") ?(reporter = ignore)
    ~identity ~ciphers ~shared_keys sockaddr =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Fiber.return (Connect.connect socket sockaddr)
  >>| reword_error (fun err -> `Connect err)
  >>? fun () ->
  Bob_unix.init_peer socket ~identity >>= function
  | Error (#Bob_unix.error as err) ->
      Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
  | Ok () -> (
      let tmp = Pack.Temp.random_temporary_path ?g tmp in
      let oc = open_out_bin (Fpath.to_string tmp) in
      let flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
      let rec go () =
        Crypto.recv flow >>= function
        | Ok (`Data str) ->
            reporter (String.length str);
            output_string oc str;
            go ()
        | Ok `End -> Crypto.close flow >>= fun () -> Fiber.return (Ok ())
        | Error _ as err -> Crypto.close flow >>= fun () -> Fiber.return err
      in
      go ()
      >>| reword_error (fun err ->
              `Crypto (err :> [ Crypto.write_error | Crypto.error ]))
      >>= fun res ->
      close_out oc;
      match res with
      | Ok () -> Fiber.return (Ok tmp)
      | Error _ as err -> Fiber.return err)
