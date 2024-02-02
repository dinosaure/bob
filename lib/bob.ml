let src = Logs.Src.create "bob.core"

module Log = (val Logs.src_log src : Logs.LOG)
module State = State
module Handshake = Handshake
module Crypto = Crypto

type 'peer income =
  | Income : ('a, 'peer) State.src * ('a, 'peer) State.packet -> 'peer income

module type PEER = sig
  type t
  type peer

  val peer : [ `Client | `Server ]
  val src_and_packet : State.src_rel -> peer income option

  val process_packet :
    t ->
    ('a, peer) State.src ->
    ('a, peer) State.packet ->
    [> `Continue
    | `Agreement of string
    | `Done of string * (Spoke.cipher * Spoke.cipher) * Spoke.shared_keys
    | `Close ]

  val next_packet : t -> (int * State.raw) option
end

let data_is_empty = function `End | `Data (_, _, 0) -> true | `Data _ -> false

module Make (Peer : PEER) = struct
  type t = {
    state : Peer.t;
    ctx : Handshake.ctx;
    mutable ic : (int * State.raw) Handshake.t;
    mutable oc : unit Handshake.t;
    mutable closed : bool;
  }

  let to_whom_I_speak =
    match Peer.peer with `Server -> `Client | `Client -> `Server

  let receive t data =
    let rec go data = function
      | Handshake.Done (uid, packet) -> (
          Log.debug (fun m ->
              m "Process a new packet from %04x: %a" uid State.pp_raw packet);
          let src_rel = State.src_and_packet ~peer:to_whom_I_speak uid packet in

          match Peer.src_and_packet src_rel with
          | Some (Income (src, packet)) -> (
              match Peer.process_packet t.state src packet with
              | `Done sk ->
                  Log.debug (fun m -> m "The peer is done.");
                  `Done sk
              | `Close ->
                  Log.debug (fun m -> m "The relay closed the connection.");
                  t.closed <- true;
                  `Close
              | `Agreement _ as agreement ->
                  Handshake.save t.ctx data;
                  t.ic <- Handshake.recv t.ctx;
                  agreement
              | `Continue ->
                  (* NOTE(dinosaure): [Handshake.recv t.ctx] has a side-effet on
                     [ctx]. We want to check that, before to process anything, we
                     have something into [ctx] and [data]. In that case and only
                     then, we process [ctx] and try to parse a packet into it.

                     In other words, **don't** factor [t.ic <- Handshake.recv t.ctx]!
                  *)
                  if Handshake.income_is_empty t.ctx && data_is_empty data then (
                    t.ic <- Handshake.recv t.ctx;
                    `Continue)
                  else (
                    t.ic <- Handshake.recv t.ctx;
                    go data t.ic))
          | None ->
              Log.warn (fun m ->
                  m "Discard a packet from %04x: %a" uid State.pp_raw packet);
              `Continue)
      | Handshake.Fail err ->
          Log.err (fun m ->
              m "Got an error while parsing: %a" Handshake.pp_error err);
          `Error err
      | Rd { buf = dst; off = dst_off; len = dst_len; k } as ic -> (
          match data with
          | `End -> go data (k `End)
          | `Data (_, _, 0) ->
              t.ic <- ic;
              `Read
          | `Data (str, off, len) ->
              let max = min dst_len len in
              Bytes.blit_string str off dst dst_off max;
              go (`Data (str, off + max, len - max)) (k (`Len max)))
      | Wr _ -> assert false
    in
    if t.closed then `Close else go data t.ic

  let send t =
    let rec go = function
      | Handshake.Done () -> (
          match Peer.next_packet t.state with
          | Some (uid, packet) -> go (Handshake.send_packet t.ctx (uid, packet))
          | None ->
              t.oc <- Done ();
              `Continue)
      | Handshake.Fail err -> `Error err
      | Wr { str; off; len; k } ->
          t.oc <- k len;
          `Write (String.sub str off len)
      | Rd _ -> assert false
    in
    go t.oc
end

module Server = struct
  include Make (struct
    type peer = State.server

    let peer = `Server

    let src_and_packet = function
      | State.Server_packet (src, packet) -> Some (Income (src, packet))
      | State.Client_packet (src, packet) ->
          let src = State.uid_of_src src in
          let packet = State.packet_to_raw packet in
          Log.warn (fun m ->
              m "Got an unexpected client (%04x) packet: %a" src State.pp_raw
                packet);
          None
      | State.Invalid_packet (uid, packet) ->
          Log.warn (fun m ->
              m "Got an invalid packet (%04x): %a" uid State.pp_raw packet);
          None

    include State.Server
  end)

  let hello ?reproduce ~g secret =
    let state = State.Server.hello ?reproduce ~g secret in
    let ctx = Handshake.make () in
    let ic = Handshake.recv ctx in
    let oc =
      match State.Server.next_packet state with
      | Some (uid, packet) -> Handshake.send_packet ctx (uid, packet)
      | None -> Handshake.Done ()
    in
    { state; ctx; ic; oc; closed = false }
end

module Client = struct
  include Make (struct
    type peer = State.client

    let peer = `Client

    let src_and_packet = function
      | State.Client_packet (src, packet) -> Some (Income (src, packet))
      | State.Server_packet (src, packet) ->
          let src = State.uid_of_src src in
          let packet = State.packet_to_raw packet in
          Log.warn (fun m ->
              m "Got an unexpected server (%04x) packet: %a" src State.pp_raw
                packet);
          None
      | State.Invalid_packet (uid, packet) ->
          Log.warn (fun m ->
              m "Got an invalid packet (%04x): %a" uid State.pp_raw packet);
          None

    include State.Client
  end)

  let make ?reproduce ~g ~identity password =
    let state = State.Client.hello ?reproduce ~g ~identity password in
    let ctx = Handshake.make () in
    let ic = Handshake.recv ctx in
    let oc = Handshake.Done () in
    { state; ctx; ic; oc; closed = false }

  let agreement t = function
    | `Accept ->
        Log.debug (fun m -> m "The client accepts the agreement.");
        State.Client.accept t.state
    | `Refuse ->
        Log.debug (fun m -> m "The client refuses the agreement.");
        State.Client.refuse t.state
end

module Relay = struct
  type t = {
    state : State.Relay.t;
    ctxs : (string, Handshake.ctx) Hashtbl.t;
    ics : (string, (int * State.raw) Handshake.t) Hashtbl.t;
    mutable k : [ `Close of string | `Continue ] Handshake.t;
    mutable peer_identity : string;
  }

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>ctxs=@[<hov>%a@];@ ics=%d;@] }"
      Fmt.(Dump.list string)
      (Hashtbl.fold (fun k _ acc -> k :: acc) t.ctxs [])
      (Hashtbl.length t.ics)

  let make () =
    {
      state = State.Relay.make ();
      ctxs = Hashtbl.create 0x100;
      ics = Hashtbl.create 0x100;
      k = Done `Continue;
      peer_identity = "\xde\xad\xbe\xef";
    }

  let new_peer t ~identity =
    let ctx = Handshake.make () in
    Hashtbl.add t.ctxs identity ctx;
    Hashtbl.add t.ics identity (Handshake.recv ctx)

  let rem_peer t ~identity = State.Relay.delete ~identity t.state
  let exists t ~identity = State.Relay.exists ~identity t.state

  let receive_from t ~identity data =
    match
      (Hashtbl.find_opt t.ics identity, Hashtbl.find_opt t.ctxs identity)
    with
    | None, None ->
        Log.err (fun m -> m "%s does not exists as an active peer" identity);
        `Close
    | None, Some _ ->
        Log.err (fun m -> m "%s does not have input anymore." identity);
        Hashtbl.remove t.ctxs identity;
        `Close
    | Some _, None ->
        Log.err (fun m -> m "%s does not have context anymore." identity);
        Hashtbl.remove t.ics identity;
        `Close
    | Some ic, Some ctx ->
        let rec go data = function
          | Handshake.Done (uid, packet) -> (
              Log.debug (fun m ->
                  m "Receive a packet from %04x: %a" uid State.pp_raw packet);
              let result =
                match
                  State.Relay.dst_and_packet
                    ~identity:(identity :> string)
                    t.state uid packet
                with
                | Relay_packet (dst, packet) ->
                    State.Relay.process_packet t.state
                      ~identity:(identity :> string)
                      dst packet
                | Invalid_packet (uid, packet) ->
                    Log.err (fun m ->
                        m "Receive an invalid packet (%04x): %a" uid
                          State.pp_raw packet);
                    `Continue
              in
              match result with
              | `Agreement _ as agreement ->
                  Handshake.save ctx data;
                  (* TODO(dinosaure): dragoon here! *)
                  Hashtbl.replace t.ics identity (Handshake.recv ctx);
                  agreement
              | `Continue ->
                  if Handshake.income_is_empty ctx && data_is_empty data then (
                    Hashtbl.replace t.ics identity (Handshake.recv ctx);
                    result)
                  else
                    let ic = Handshake.recv ctx in
                    Hashtbl.replace t.ics identity ic;
                    go data ic)
          | Handshake.Fail err ->
              Log.err (fun m ->
                  m "Got an error from %s: %a" identity Handshake.pp_error err);
              Hashtbl.remove t.ics identity;
              Hashtbl.remove t.ctxs identity;
              `Close
          | Rd { buf = dst; off = dst_off; len = dst_len; k } as ic -> (
              match data with
              | `End -> go data (k `End)
              | `Data (_, _, 0) ->
                  Hashtbl.replace t.ics identity ic;
                  `Read
              | `Data (str, off, len) ->
                  let max = min dst_len len in
                  Bytes.blit_string str off dst dst_off max;
                  go (`Data (str, off + max, len - max)) (k (`Len max)))
          | Wr _ -> assert false
        in
        go data ic

  let rec send_to t =
    match t.k with
    | Handshake.Rd _ -> assert false
    | Handshake.Fail err ->
        Log.err (fun m ->
            m "Got an error from %s: %a" t.peer_identity Handshake.pp_error err);
        Hashtbl.remove t.ctxs t.peer_identity;
        `Close t.peer_identity
    | Handshake.Wr { str; off; len; k } ->
        t.k <- k len;
        `Write (t.peer_identity, String.sub str off len)
    | Handshake.Done (`Close identity) ->
        t.k <- Handshake.Done `Continue;
        `Close identity
    | Handshake.Done `Continue -> (
        match State.Relay.next_packet t.state with
        | Some (identity, uid, (`Done as packet))
        | Some (identity, uid, (`Accepted as packet))
        | Some (identity, uid, (`Timeout as packet)) -> (
            match Hashtbl.find_opt t.ctxs identity with
            | None -> `Close identity
            | Some ctx ->
                t.peer_identity <- identity;
                t.k <-
                  Handshake.(
                    send_packet ctx (uid, packet) >>= fun () ->
                    Hashtbl.remove t.ics identity;
                    Hashtbl.remove t.ctxs identity;
                    return (`Close identity));
                Log.debug (fun m -> m "Send the last packet to %s" identity);
                send_to t)
        | Some (identity, uid, packet) -> (
            match Hashtbl.find_opt t.ctxs identity with
            | None -> `Close identity
            | Some ctx ->
                t.peer_identity <- identity;
                t.k <-
                  Handshake.(
                    send_packet ctx (uid, packet) >>= fun () -> return `Continue);
                send_to t)
        | None -> `Continue)
end

module Secured = struct
  type t = (string, string) Hashtbl.t

  let make () = Hashtbl.create 0x100

  let add_peers t peer0 peer1 =
    Hashtbl.add t peer0 peer1;
    Hashtbl.add t peer1 peer0

  let rem_peers t peer0 peer1 =
    Hashtbl.remove t peer0;
    Hashtbl.remove t peer1

  type reader =
    [ `Data of string * int * int | `End ] ->
    [ `Continue of 'k | `Invalid_peer | `Peer of string * string ]
    as
    'k

  let rec reader t =
    ();
    let tmp = Bytes.make 100 '\000' in
    let pos = ref 0 in
    go t tmp pos

  and go t tmp pos = function
    | `End -> `Invalid_peer
    | `Data (str, off, len) ->
        if !pos + len > Bytes.length tmp then `Invalid_peer
        else (
          Bytes.blit_string str off tmp !pos len;
          pos := !pos + len;
          match Bytes.index_opt tmp '\n' with
          | None -> `Continue (go t tmp pos)
          | Some pos -> (
              let peer0 = Bytes.sub_string tmp 0 pos in
              match Hashtbl.find_opt t peer0 with
              | Some peer1 -> `Peer (peer0, peer1)
              | None -> `Invalid_peer))
end
