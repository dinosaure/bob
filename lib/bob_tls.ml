open Fiber

let rec full_write fd str ~off ~len =
  Fiber.write fd str ~off ~len >>= function
  | Error _ as err -> Fiber.return err
  | Ok len' ->
      if len - len' > 0 then
        full_write fd str ~off:(off + len') ~len:(len - len')
      else Fiber.return (Ok ())

type error =
  [ `Closed
  | `Unix of Unix.error
  | `Alert of Tls.Packet.alert_type
  | `Failure of Tls.Engine.failure ]

let pp_error ppf = function
  | `Closed -> Fmt.string ppf "Connection closed by peer"
  | `Unix errno -> Fmt.string ppf (Unix.error_message errno)
  | `Alert alert -> Fmt.string ppf (Tls.Packet.alert_type_to_string alert)
  | `Failure failure -> Fmt.string ppf (Tls.Engine.string_of_failure failure)

exception Tls of error

type t = {
  fd : Unix.file_descr;
  mutable state : [ `Active of Tls.Engine.state | `End | `Error of error ];
  mutable linger : Cstruct.t option;
}

let rec read_react t =
  let handle tls buf =
    match Tls.Engine.handle_tls tls buf with
    | Error (failure, `Response resp) -> (
        t.state <- `Error (`Failure failure);
        let { Cstruct.buffer; off; len } = resp in
        full_write t.fd buffer ~off ~len >>= function
        | Ok () -> read_react t
        | Error err ->
            t.state <- `Error (err :> error);
            Fiber.return (`Error (err :> error)))
    | Ok (state', `Response resp, `Data data) -> (
        let state' =
          match state' with
          | `Ok tls -> `Active tls
          | `Eof -> `End
          | `Alert alert -> `Error (`Alert alert)
        in
        t.state <- state';
        match resp with
        | None -> Fiber.return (`Ok data)
        | Some resp -> (
            let { Cstruct.buffer; off; len } = resp in
            full_write t.fd buffer ~off ~len >>= function
            | Ok () -> Fiber.return (`Ok data)
            | Error err ->
                t.state <- `Error (err :> error);
                Fiber.return (`Error (err :> error))))
  in
  match t.state with
  | `Error err -> Fiber.return (`Error err)
  | `End -> Fiber.return `End
  | `Active _ -> (
      Fiber.read t.fd >>= fun data ->
      match (t.state, data) with
      | `Active _, Ok `End ->
          t.state <- `End;
          Fiber.return `End
      | `Active tls, Ok (`Data bstr) ->
          let cs = Cstruct.of_bigarray bstr in
          handle tls cs
      | _, Error errno ->
          t.state <- `Error (`Unix errno);
          Fiber.return (`Error (`Unix errno))
      | `Error err, _ -> Fiber.return (`Error err)
      | `End, _ -> Fiber.return `End)

let rec read t buf =
  let write_out res =
    let len = Cstruct.length res in
    let max = min (Cstruct.length buf) len in
    Cstruct.blit res 0 buf 0 max;
    t.linger <-
      (if max < len then Some (Cstruct.sub res max (len - max)) else None);
    Fiber.return max
  in
  match t.linger with
  | Some res -> write_out res >>| fun v -> Ok v
  | None -> (
      read_react t >>= function
      | `End -> Fiber.return (Ok 0)
      | `Ok None -> read t buf >>? fun v -> Fiber.return (Ok v)
      | `Ok (Some res) -> write_out res >>| fun v -> Ok v
      | `Error err -> Fiber.return (Error err))

let writev t css =
  match t.state with
  | `Error err -> raise (Tls err)
  | `End -> raise (Tls `Closed)
  | `Active tls -> (
      match Tls.Engine.send_application_data tls css with
      | None -> Fmt.invalid_arg "Socket is not ready"
      | Some (tls, data) -> (
          t.state <- `Active tls;
          let { Cstruct.buffer; off; len } = data in
          full_write t.fd buffer ~off ~len >>= function
          | Ok () -> Fiber.return ()
          | Error err ->
              t.state <- `Error (err :> error);
              raise (Tls (err :> error))))

let write t cs = writev t [ cs ]

let rec drain_handshake t =
  let push_linger t mcs =
    match (mcs, t.linger) with
    | None, _ -> ()
    | scs, None -> t.linger <- scs
    | Some cs, Some linger -> t.linger <- Some (Cstruct.append linger cs)
  in
  match t.state with
  | `Active tls when not (Tls.Engine.handshake_in_progress tls) ->
      Fiber.return (Ok t)
  | _ -> (
      read_react t >>= function
      | `Error err -> Fiber.return (Error err)
      | `End -> Fiber.return (Error `Closed)
      | `Ok cs ->
          push_linger t cs;
          drain_handshake t)

let close t =
  match t.state with
  | `Active tls -> (
      let _, { Cstruct.buffer; off; len } = Tls.Engine.send_close_notify tls in
      t.state <- `End;
      full_write t.fd buffer ~off ~len >>= function
      | Ok () -> Fiber.close t.fd
      | Error err ->
          t.state <- `Error (err :> error);
          Fiber.close t.fd >>= fun () -> raise (Tls (err :> error)))
  | _ -> Fiber.close t.fd

let client_of_file_descr config ?host fd =
  let config =
    match host with None -> config | Some host -> Tls.Config.peer config host
  in
  let t = { state = `End; fd; linger = None } in
  let tls, { Cstruct.buffer; off; len } = Tls.Engine.client config in
  let t = { t with state = `Active tls } in
  full_write t.fd buffer ~off ~len >>= function
  | Ok () -> Fiber.catch (fun () -> drain_handshake t) (fun exn -> raise exn)
  | Error err ->
      t.state <- `Error (err :> error);
      raise (Tls (err :> error))
