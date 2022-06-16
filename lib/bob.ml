module State = State
module Protocol = Protocol

type 'peer income =
  | Income : ('a, 'peer) State.src * ('a, 'peer) State.packet -> 'peer income

module type PEER = sig
  type t
  type peer

  val peer : [ `Client | `Server ]
  val src_and_packet : State.src_rel -> peer income option
  val process_packet : t -> ('a, peer) State.src -> ('a, peer) State.packet ->
    [> `Continue | `Done of Spoke.shared_keys | `Close ]
  val next_packet : t -> (int * State.raw) option
end

module Make (Peer : PEER) = struct
  type t =
    { state : Peer.t
    ; ctx : Protocol.ctx
    ; mutable ic : (int * State.raw) Protocol.t
    ; mutable oc : unit Protocol.t
    ; mutable closed : bool }

  let to_whom_I_speak = match Peer.peer with
    | `Server -> `Client
    | `Client -> `Server
  let receive t data =
    let rec go data = function
      | Protocol.Done (uid, packet) ->
        t.ic <- Protocol.recv t.ctx ;
        let src_rel = State.src_and_packet ~peer:to_whom_I_speak uid packet in

        ( match Peer.src_and_packet src_rel with
        | Some (Income (src, packet)) ->
          let res = Peer.process_packet t.state src packet in
          t.closed <- res = `Close ; res
        | None -> `Continue )
      | Protocol.Fail err -> `Error err
      | Rd { buf= dst; off= dst_off; len= dst_len; k; } as ic ->
        ( match data with
        | `End -> go data (k `End)
        | `Data (  _,   _, 0) -> t.ic <- ic ; `Read
        | `Data (str, off, len) ->
          let max = min dst_len len in
          Bytes.blit_string str off dst dst_off max ;
          go (`Data (str, off + max, len - max)) (k (`Len max)) )
      | Wr _ -> assert false in
    if t.closed then `Close else go data t.ic

  let send t =
    let rec go = function
      | Protocol.Done () ->
        ( match Peer.next_packet t.state with
        | Some (uid, packet) -> go (Protocol.send_packet t.ctx (uid, packet))
        | None -> t.oc <- Done () ; `Continue )
      | Protocol.Fail err -> `Error err
      | Wr { str; off; len; k; } ->
        t.oc <- k len ; `Write (String.sub str off len)
      | Rd _ -> assert false in
    go t.oc
end

module Server = struct
  include Make (struct
    type peer = State.server
    let peer = `Server
    let src_and_packet = function
      | State.Server_packet (src, packet) -> Some (Income (src, packet))
      | _ -> None

    include State.Server end)

  let hello ~g ~secret =
    let state = State.Server.hello ~g ~secret in
    let ctx = Protocol.make () in
    let ic = Protocol.recv ctx in
    let oc = match State.Server.next_packet state with
      | Some (uid, packet) -> Protocol.send_packet ctx (uid, packet)
      | None -> Protocol.Done () in
    { state; ctx; ic; oc; closed= false; }
end

module Client = struct
  include Make (struct
    type peer = State.client
    let peer = `Client
    let src_and_packet = function
      | State.Client_packet (src, packet) -> Some (Income (src, packet))
      | _ -> None

    include State.Client end)

  let make ~g ~password ~identity =
    let state = State.Client.make ~g ~password ~identity in
    let ctx = Protocol.make () in
    let ic = Protocol.recv ctx in
    let oc = Protocol.Done () in
    { state; ctx; ic; oc; closed= false; }
end
