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
  mutable state :
    [ `Active of Tls.Engine.state
    | `Closed
    | `Read_closed of Tls.Engine.state
    | `Write_closed of Tls.Engine.state
    | `Error of error ];
  mutable linger : Cstruct.t option;
}

let half_close state mode =
  match (state, mode) with
  | `Active tls, `read -> `Read_closed tls
  | `Active tls, `write -> `Write_closed tls
  | `Active _, `read_write -> `Closed
  | `Read_closed tls, `read -> `Read_closed tls
  | `Read_closed _, (`write | `read_write) -> `Closed
  | `Write_closed tls, `write -> `Write_closed tls
  | `Write_closed _, (`read | `read_write) -> `Closed
  | ((`Closed | `Error _) as e), (`read | `write | `read_write) -> e

let inject_state tls = function
  | `Active _ -> `Active tls
  | `Read_closed _ -> `Read_closed tls
  | `Write_closed _ -> `Write_closed tls
  | (`Closed | `Error _) as e -> e

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
    | Ok (state', eof, `Response resp, `Data data) -> (
        let state' = inject_state state' t.state in
        let state' =
          Stdlib.Option.(
            value ~default:state'
              (map (fun `Eof -> half_close state' `read) eof))
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
  | `Read_closed _ | `Closed -> Fiber.return `End
  | `Active _ | `Write_closed _ -> (
      Fiber.read t.fd >>= function
      | Error errno ->
          t.state <- `Error (`Unix errno);
          Fiber.return (`Error (`Unix errno))
      | Ok `End ->
          t.state <- half_close t.state `read;
          Fiber.return `End
      | Ok (`Data bstr) -> (
          let cs = Cstruct.of_bigarray bstr in
          match t.state with
          | `Active tls | `Write_closed tls -> handle tls cs
          | `Read_closed _ | `Closed -> Fiber.return `End
          | `Error _ as e -> Fiber.return e))

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
  | `Error err -> Fiber.return (Error err)
  | `Closed | `Write_closed _ -> Fiber.return (Error `Closed)
  | `Active tls | `Read_closed tls -> (
      match Tls.Engine.send_application_data tls css with
      | None -> Fmt.invalid_arg "Socket is not ready"
      | Some (tls, data) -> (
          t.state <- `Active tls;
          let { Cstruct.buffer; off; len } = data in
          full_write t.fd buffer ~off ~len >>= function
          | Ok () -> Fiber.return (Ok ())
          | Error err ->
              t.state <- `Error (err :> error);
              Fiber.return (Error (err :> error))))

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
  | `Active tls | `Read_closed tls -> (
      let tls, { Cstruct.buffer; off; len } =
        Tls.Engine.send_close_notify tls
      in
      t.state <- inject_state tls t.state;
      t.state <- `Closed;
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
  let tls, { Cstruct.buffer; off; len } = Tls.Engine.client config in
  let t = { state = `Active tls; fd; linger = None } in
  full_write t.fd buffer ~off ~len >>= function
  | Ok () -> Fiber.catch (fun () -> drain_handshake t) (fun exn -> raise exn)
  | Error err ->
      t.state <- `Error (err :> error);
      raise (Tls (err :> error))
