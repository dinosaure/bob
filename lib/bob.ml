let src = Logs.Src.create "bob.core"
module Log = (val Logs.src_log src : Logs.LOG)

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
    [> `Continue | `Agreement of string | `Done of string * Spoke.shared_keys | `Close ]
  val next_packet : t -> (int * State.raw) option
end

let data_is_empty = function
  | `End | `Data (_, _, 0) -> true
  | `Data _ -> false

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
        Log.debug (fun m -> m "Process a new packet from %04x: %a"
          uid State.pp_raw packet) ;
        let src_rel = State.src_and_packet ~peer:to_whom_I_speak uid packet in

        ( match Peer.src_and_packet src_rel with
        | Some (Income (src, packet)) ->
          ( match Peer.process_packet t.state src packet with
          | `Done sk -> `Done sk
          | `Close -> t.closed <- true ; `Close
          | `Agreement _ as agreement ->
            Protocol.save t.ctx data ;
            t.ic <- Protocol.recv t.ctx ;
            agreement
          | `Continue ->
            (* NOTE(dinosaure): [Protocol.recv t.ctx] has a side-effet on
               [ctx]. We want to check that, before to process anything, we
               have something into [ctx] and [data]. In that case and only
               then, we process [ctx] and try to parse a packet into it.

               In other words, **don't** factor [t.ic <- Protocol.recv t.ctx]!
             *)
            if Protocol.income_is_empty t.ctx && data_is_empty data
            then ( t.ic <- Protocol.recv t.ctx ; `Continue )
            else ( t.ic <- Protocol.recv t.ctx ; go data t.ic ) )
        | None -> Log.warn (fun m -> m "Discard a packet from %04x: %a"
          uid State.pp_raw packet) ; `Continue )
      | Protocol.Fail err -> `Error err
      | Rd { buf= dst; off= dst_off; len= dst_len; k; } as ic ->
        ( match data with
        | `End -> go data (k `End)
        | `Data (  _,   _, 0) ->
          t.ic <- ic ; `Read
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
      | State.Client_packet (src, packet) ->
        let src = State.uid_of_src src in
        let packet = State.packet_to_raw packet in
        Log.warn (fun m -> m "Got an unexpected client (%04x) packet: %a"
          src State.pp_raw packet) ;
        None
      | State.Invalid_packet (uid, packet) ->
        Log.warn (fun m -> m "Got an invalid packet (%04x): %a"
          uid State.pp_raw packet) ; None

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
      | State.Server_packet (src, packet) ->
        let src = State.uid_of_src src in
        let packet = State.packet_to_raw packet in
        Log.warn (fun m -> m "Got an unexpected server (%04x) packet: %a"
          src State.pp_raw packet) ;
        None
      | State.Invalid_packet (uid, packet) ->
        Log.warn (fun m -> m "Got an invalid packet (%04x): %a"
          uid State.pp_raw packet) ; None

    include State.Client end)

  let make ~g ~password ~identity =
    let state = State.Client.hello ~g ~password ~identity in
    let ctx = Protocol.make () in
    let ic = Protocol.recv ctx in
    let oc = Protocol.Done () in
    { state; ctx; ic; oc; closed= false; }

  let agreement t = function
    | `Accept ->
      Log.debug (fun m -> m "The client accepts the agreement.") ;
      State.Client.accept t.state 
    | `Refuse ->
      Log.debug (fun m -> m "The client refuses the agreement.") ;
      State.Client.refuse t.state
end

module Relay = struct
  type t =
    { state : State.Relay.t
    ; ctxs  : (string, Protocol.ctx) Hashtbl.t
    ; ics   : (string, (int * State.raw) Protocol.t) Hashtbl.t
    ; ocs   : (string, unit Protocol.t) Hashtbl.t }

  let make () =
    { state= State.Relay.make ()
    ; ctxs=  Hashtbl.create 0x100
    ; ics=   Hashtbl.create 0x100
    ; ocs=   Hashtbl.create 0x100 }

  let remove_if_it_exists t ~identity =
    match Hashtbl.find_opt t identity with
    | Some _ -> Hashtbl.remove t identity | None -> ()

  let new_peer t ~identity =
    let ctx = Protocol.make () in
    Hashtbl.add t.ctxs identity ctx ;
    Hashtbl.add t.ics identity (Protocol.recv ctx)

  let rem_peer t ~identity =
    State.Relay.delete ~identity t.state

  let exists t ~identity = State.Relay.exists ~identity t.state

  let receive_from t ~identity data =
    match Hashtbl.find_opt t.ics identity,
          Hashtbl.find_opt t.ctxs identity with
    | None,    None   ->
      Log.err (fun m -> m "%s does not exists as an active peer"
        (identity :> string)) ; `Close
    | None,    Some _ ->
      Log.err (fun m -> m "%s does not have input anymore."
        (identity :> string)) ;
      remove_if_it_exists t.ctxs ~identity ; `Close
    | Some _,  None   ->
      Log.err (fun m -> m "%s does not have context anymore."
        (identity :> string)) ;
      remove_if_it_exists t.ics  ~identity ; `Close
    | Some ic, Some ctx ->
      let rec go data = function
        | Protocol.Done (uid, packet) ->
          Log.debug (fun m -> m "Receive a packet from %04x: %a"
            uid State.pp_raw packet) ;
          let result = match State.Relay.dst_and_packet
              ~identity:(identity :> string) t.state uid packet with
            | Relay_packet (dst, packet) ->
              State.Relay.process_packet t.state ~identity:(identity :> string)
                dst packet
            | Invalid_packet (uid, packet) ->
              Log.err (fun m -> m "Receive an invalid packet (%04x): %a"
                uid State.pp_raw packet) ;
              `Continue in
          ( match result with
          | `Agreement _ as agreement ->
            Protocol.save ctx data ;
            Hashtbl.replace t.ics identity (Protocol.recv ctx) ; agreement
          | `Continue ->
            if Protocol.income_is_empty ctx && data_is_empty data
            then ( Hashtbl.replace t.ics identity (Protocol.recv ctx) 
                 ; result )
            else ( let ic = Protocol.recv ctx in
                   Hashtbl.replace t.ics identity ic ;
                   go data ic ) )
        | Protocol.Fail err ->
          Log.err (fun m -> m "Got an error from %s: %a"
            (identity :> string) Protocol.pp_error err) ;
          remove_if_it_exists t.ics  ~identity ;
          remove_if_it_exists t.ctxs ~identity ;
          `Close
        | Rd { buf= dst; off= dst_off; len= dst_len; k; } as ic ->
          ( match data with
          | `End -> go data (k `End)
          | `Data (  _,   _, 0) ->
            Hashtbl.replace t.ics identity ic ; `Read
          | `Data (str, off, len) ->
            let max = min dst_len len in
            Bytes.blit_string str off dst dst_off max ;
            go (`Data (str, off + max, len - max)) (k (`Len max)) ) 
        | Wr _ -> assert false in
      go data ic

  let rec send_to t =
    match State.Relay.next_packet t.state with
    | Some (identity, uid, packet) ->
      ( match Hashtbl.find_opt t.ctxs identity with
      | None ->
        Log.warn (fun m -> m "[%04x]%s is not an active connection."
          uid identity) ;
        `Close identity
      | Some ctx ->
        go t ~identity (Protocol.send_packet ctx (uid, packet)) )
    | None -> `Continue
  and go t ~identity = function
    | Protocol.Done () ->
      Hashtbl.replace t.ocs identity (Done ()) ;
      send_to t
    | Protocol.Fail err ->
      Log.err (fun m -> m "Got an error from %s: %a"
        identity Protocol.pp_error err) ;
      remove_if_it_exists t.ocs  ~identity ;
      remove_if_it_exists t.ctxs ~identity ;
      `Close (identity :> string)
    | Wr { str; off; len; k; } ->
      Hashtbl.replace t.ocs identity (k len) ;
      `Write (identity, String.sub str off len)
    | Rd _ -> assert false
end
