(* XXX(dinosaure):
   rational:
   - a client can only communicate to a server or a relay
   - a server can only communicate to a client or a relay
   - a relay can communicate to anyone

   With GADTs, we can encore these properties and discard many cases that
   - the client should never handle
   - the server should never handle
   - the relay  should not introspect ("blind" mode)

   The exchange:
   [server] has a [public] value
   [client] has only the [password]

   Even if we talk about peers and a relay, we differentiate a client
   from a server. The first waits something, the second wants to send
   something - at the end, we should have a communication from the
   server to the client ([server ~> client]).

   1) Agreement on identities

   To get a state where:
   - A client has an identity which is know by the relay and servers
   - A server has an identity which is know by the relay and clients
   We let the relay to be the oracle about identities.

   [server] --> [Hello_as_a_server { public; }] --> [relay]
   [server] <--      [Server_identity str]      <-- [relay]

   [client] -->       [Hello_as_a_client]       --> [relay]
   [client] <--      [Client_identity str]      <-- [relay]

   2) A new server => broadcast its existence to all clients

   When the relay receives a new server, it will send a [New_server] to any
   clients and at the same time send the identity to the server:

   [server] --> [Hello_as_a_server { public; }] --> [relay]
   [server] <--      [Server_identity str]      <-- [relay]
   [  c0  ] <--          [New_server _]         <-- [relay]
            ==> start the handshake with [server]
   [  c1  ] <--          [New_server _]         <-- [relay]
            ==> start the handshake with [server]
     ....
   [  cN  ] <--          [New_server _]         <-- [relay]
            ==> start the handshake with [server]

   [New_server _] starts the handshake on the client side. A "blind"
   communication between the new server and clients can begin. Any errors
   while the handshake is reported to the **specific** client or the
   **specific** server. In such case, these peers are removed as possible
   candidates on both sides.

   3) A new client => broadcast its existence to all servers

   When the relay receives a new client, it will **respond** to it with all
   available servers:
   [client] --> [Hello_as_a_client] --> [relay]
   [client] <--   [New_server s0]   <-- [relay]
   [client] <--   [New_server s1]   <-- [relay]
     ....
   [client] <--   [New_server sN]   <-- [relay]

   The reception of one [New_server _] asks the [client] to start an handshake
   to the specific [server]. A "blind" communication between servers and the
   new client can begin. Any errors while the handshake is reported to the
   **specific** client or the the **specific** server. In such case, these
   peers are remove as possible candidates on both sides.

   4) The handshake

   The client will send [X] and its identity (sent by the relay at the
   beginning) to available servers. It will get so some errors or some valid
   responses [Y_and_server_validator]. In the last case, it will send the
   [Client_validator] and it will wait an [Agreement] from a server.

   [client] -->  [X_and_client_identity _]  --> [server] (via [relay])
   [client] <-- [Y_and_server_validator _ ] <-- [server] (via [relay])
   [client] -->    [Client_validator _ ]    --> [server] (via [relay])
   [client] <--         [Agreement]         <-- [server] (via [relay])

   At this stage, a [client] and a [server] have an agreement about a
   shared key. However, only the [client] is able (via an end-user income) to
   accept the agreement or not:

   $ do you accept the agreement with [server]?
     Yes => [client] --> [Accepted] --> [server] (via [relay])
     No  => [client] --> [Refused ] --> [server] (via [relay])

   5) Finalization

   The relay introspects only [Accepted] and [Refused]. If the client accepted
   the agreement, [client] and [server] are deleted as active connection to
   the relay and a room should be allocated for them then.

   Otherwise ([Refused] case), only candidates are deleted and connection
   are kept until a new agreement (or a timeout).

   5) Timeout

   The relay keeps connections until a certain time. It can decide to delete
   a candidate because it took too much times to find a candidate. In such
   case (the worst case), the relay emits a [Closed] packet to the peer and
   it notices candidates of this peer that the latter is no longer available.
*)

let src = Logs.Src.create "state"

module Log = (val Logs.src_log src : Logs.LOG)

type relay = |
and client = |
and server = |

type ('a, 'b) peer =
  | Server : (server, client) peer
  | Client : (client, server) peer

type ('f, 'r, 't) transmit =
  | Blind : ('f, 't, 't) transmit
  | From_client : (client, server, relay) transmit
  | From_server : (server, client, relay) transmit

type ('f, 't) src =
  | Relay : (relay, 't) src
  | Peer : ('f, 'nf) peer * int -> ('f, 'nf) src

let uid_of_src : type f t. (f, t) src -> int = function
  | Relay -> 0
  | Peer (_, uid) -> uid

type ('f, 't) dst =
  | Relay : ('f, relay) dst
  | Peer : ('t, 'nt) peer * int -> ('nt, 't) dst

let uid_of_dst : type f t. (f, t) dst -> int = function
  | Relay -> 0
  | Peer (_, uid) -> uid

let to_relay = Relay
let to_server ~uid = Peer (Server, uid)
let to_client ~uid = Peer (Client, uid)

type ('f, 't) packet =
  | Hello_as_a_server : { public : Spoke.public } -> (server, relay) packet
  | Hello_as_a_client : (client, relay) packet
  | Server_identity : string -> (relay, server) packet
  | Client_identity : string -> (relay, client) packet
  | New_server : {
      uid : int;
      public : Spoke.public;
      identity : string;
    }
      -> (relay, client) packet
  | Client_validator : string -> (client, server) packet
  | Y_and_server_validator : {
      _Y : string;
      server_validator : string;
    }
      -> (server, client) packet
  | X_and_client_identity : {
      _X : string;
      identity : string;
    }
      -> (client, server) packet
  | Agreement : (server, client) packet
  | Closed : ('f, 't) src -> (relay, 't) packet
  | Accepted : (client, server) packet
  | Refused : (client, server) packet
  | Relay_failure :
      ('f, 't) peer
      * [ `Invalid_client of int
        | `Invalid_server of int
        | `No_handshake_with of int
        | `No_agreement ]
      -> ('f, relay) packet
  | Spoke_failure : ('f, 't) peer * Spoke.error -> ('f, 't) packet
  | Done : (relay, client) packet
  | Timeout : ('t, _) peer -> (relay, 't) packet

type raw =
  [ `Hello_as_a_server of Spoke.public
  | `Hello_as_a_client
  | `Server_identity of string
  | `Client_identity of string
  | `New_server of int * Spoke.public * string
  | `Client_validator of string
  | `Y_and_server_validator of string * string
  | `X_and_client_identity of string * string
  | `Agreement
  | `Closed of int
  | `Accepted
  | `Refused
  | `Relay_failure of
    [ `Invalid_client of int
    | `Invalid_server of int
    | `No_handshake_with of int
    | `No_agreement ]
  | `Spoke_failure of Spoke.error
  | `Done
  | `Timeout ]

let to_hex str =
  let res = Bytes.create (String.length str * 2) in
  let int_to_hex n =
    if n < 10 then Char.chr (n + Char.code '0')
    else Char.chr (n - 10 + Char.code 'a')
  in
  for i = 0 to String.length str - 1 do
    let n = Char.code str.[i] in
    Bytes.set res (2 * i) (int_to_hex ((n land 0xf0) lsr 4));
    Bytes.set res ((2 * i) + 1) (int_to_hex (n land 0x0f))
  done;
  Bytes.unsafe_to_string res

let pp_raw ppf = function
  | `Hello_as_a_server public ->
      Fmt.pf ppf "@{<1>(Hello_as_a_server@ %s)@]"
        (to_hex (Spoke.public_to_string public))
  | `Hello_as_a_client -> Fmt.pf ppf "Hello_as_a_client"
  | `Server_identity str -> Fmt.pf ppf "@[<1>(Server_identity@ %s)@]" str
  | `Client_identity str -> Fmt.pf ppf "@[<1>(Client_identity@ %s)@]" str
  | `New_server (uid, public, identity) ->
      Fmt.pf ppf "@[<1>(New_server@ %04d@ %s@ %s)@]" uid
        (to_hex (Spoke.public_to_string public))
        identity
  | `Client_validator _ -> Fmt.pf ppf "Client_validator"
  | `Y_and_server_validator _ -> Fmt.pf ppf "Y_and_server_validator"
  | `X_and_client_identity _ -> Fmt.pf ppf "X_and_client_identity"
  | `Agreement -> Fmt.pf ppf "Agreement"
  | `Closed uid -> Fmt.pf ppf "@[<1>(Closed@ %04x)@]" uid
  | `Accepted -> Fmt.pf ppf "Accepted"
  | `Refused -> Fmt.pf ppf "Refused"
  | `Relay_failure _ -> Fmt.pf ppf "Relay_failure"
  | `Spoke_failure _ -> Fmt.pf ppf "Spoke_failure"
  | `Done -> Fmt.pf ppf "Done"
  | `Timeout -> Fmt.pf ppf "Timeout"

let packet_to_raw : type f t. (f, t) packet -> raw = function
  | Hello_as_a_server { public } -> `Hello_as_a_server public
  | Hello_as_a_client -> `Hello_as_a_client
  | Server_identity identity -> `Server_identity identity
  | Client_identity identity -> `Client_identity identity
  | New_server { uid; public; identity } -> `New_server (uid, public, identity)
  | Client_validator validator -> `Client_validator validator
  | Y_and_server_validator { _Y; server_validator } ->
      `Y_and_server_validator (_Y, server_validator)
  | X_and_client_identity { _X; identity } ->
      `X_and_client_identity (_X, identity)
  | Agreement -> `Agreement
  | Closed (Peer (Client, uid)) | Closed (Peer (Server, uid)) -> `Closed uid
  | Accepted -> `Accepted
  | Refused -> `Refused
  | Relay_failure (Client, err) | Relay_failure (Server, err) ->
      `Relay_failure err
  | Spoke_failure (_, err) -> `Spoke_failure err
  | Closed Relay -> `Closed 000
  | Done -> `Done
  | Timeout Client -> `Timeout
  | Timeout Server -> `Timeout

type 'f send =
  | Send_to : ('f, 't) dst * ('f, 't) packet -> 'f send
  | Respond : (_, 't) dst * (relay, 't) packet -> relay send
  | Transmit : ('f, 't) src * ('f, 't) dst * ('f, 't) packet -> relay send

let send_to dst packet queue = Queue.push (Send_to (dst, packet)) queue
let respond dst packet queue = Queue.push (Respond (dst, packet)) queue

let transmit ~src ~dst packet queue =
  Queue.push (Transmit (src, dst, packet)) queue

type src_rel =
  | Server_packet : ('a, server) src * ('a, server) packet -> src_rel
  | Client_packet : ('a, client) src * ('a, client) packet -> src_rel
  | Invalid_packet of int * raw

let src_and_packet ~peer uid packet =
  match (uid, packet) with
  | 00, `Server_identity identity ->
      Server_packet (Relay, Server_identity identity)
  | 00, `Client_identity identity ->
      Client_packet (Relay, Client_identity identity)
  | 00, `New_server (uid, public, identity) ->
      Client_packet (Relay, New_server { uid; public; identity })
  | 00, `Closed 000 -> (
      match peer with
      | `Server -> Client_packet (Relay, Closed Relay)
      | `Client -> Server_packet (Relay, Closed Relay))
  | 00, `Closed uid -> (
      match peer with
      | `Server -> Client_packet (Relay, Closed (Peer (Server, uid)))
      | `Client -> Server_packet (Relay, Closed (Peer (Client, uid))))
  | 00, `Done -> Client_packet (Relay, Done)
  | 00, `Timeout -> (
      match peer with
      | `Server -> Client_packet (Relay, Timeout Client)
      | `Client -> Server_packet (Relay, Timeout Server))
  | uid, `Client_validator validator ->
      Server_packet (Peer (Client, uid), Client_validator validator)
  | uid, `Y_and_server_validator (_Y, server_validator) ->
      Client_packet
        (Peer (Server, uid), Y_and_server_validator { _Y; server_validator })
  | uid, `X_and_client_identity (_X, identity) ->
      Server_packet (Peer (Client, uid), X_and_client_identity { _X; identity })
  | uid, `Agreement -> Client_packet (Peer (Server, uid), Agreement)
  | uid, `Accepted -> Server_packet (Peer (Client, uid), Accepted)
  | uid, `Refused -> Server_packet (Peer (Client, uid), Refused)
  | uid, `Spoke_failure err -> (
      match peer with
      | `Client ->
          Server_packet (Peer (Client, uid), Spoke_failure (Client, err))
      | `Server ->
          Client_packet (Peer (Server, uid), Spoke_failure (Server, err)))
  | uid, raw -> Invalid_packet (uid, raw)

module Server = struct
  type t = {
    secret : Spoke.secret;
    mutable identity : string;
    clients : (int, Spoke.server * string) Hashtbl.t;
    g : Random.State.t;
    mutable shared_keys : (int * Spoke.shared_keys) option;
    queue : server send Queue.t;
    reproduce : bool;
  }

  let next_packet t =
    match Queue.pop t.queue with
    | Send_to (Peer (Client, uid), packet) -> Some (uid, packet_to_raw packet)
    | Send_to (Relay, packet) -> Some (000, packet_to_raw packet)
    | exception _ -> None

  let hello ?(reproduce = false) ~g secret =
    let t =
      {
        secret;
        identity = "";
        clients = Hashtbl.create 0x10;
        g;
        shared_keys = None;
        queue = Queue.create ();
        reproduce;
      }
    in
    let public = Spoke.public_of_secret secret in
    send_to to_relay (Hello_as_a_server { public }) t.queue;
    t

  let process_packet :
      type a.
      t ->
      (a, server) src ->
      (a, server) packet ->
      [> `Continue
      | `Identity of string
      | `Done of string * (Spoke.cipher * Spoke.cipher) * Spoke.shared_keys
      | `Close ] =
   fun t source packet ->
    match (source, packet) with
    (* XXX(dinosaure): impossible cases
       - the relay sent a [spoke] error
    *)
    | Relay, Spoke_failure _ -> .
    | Peer (Client, uid), Spoke_failure (Client, err) ->
        (match Hashtbl.find_opt t.clients uid with
        | Some _ ->
            Log.debug (fun m -> m "[%04x]: %a" uid Spoke.pp_error err);
            Hashtbl.remove t.clients uid
        | None -> Log.warn (fun m -> m "[%04x] does not exist" uid));
        `Continue
    | Relay, Server_identity identity ->
        t.identity <- identity;
        `Identity identity
    | Peer (Client, uid), X_and_client_identity { _X; identity } -> (
        let identities =
          match t.reproduce with
          | true -> ("foo", "bar")
          | false -> (identity, t.identity)
        in
        match
          Spoke.server_compute ~g:t.g ~secret:t.secret ~identity:identities _X
        with
        | Ok (state, (_Y, server_validator)) ->
            Hashtbl.add t.clients uid (state, identity);
            let packet = Y_and_server_validator { _Y; server_validator } in
            send_to (to_client ~uid) packet t.queue;
            `Continue
        | Error err ->
            send_to (to_client ~uid) (Spoke_failure (Server, err)) t.queue;
            `Continue)
    | Peer (Client, uid), Client_validator client_validator -> (
        match Hashtbl.find_opt t.clients uid with
        | None ->
            let err = Relay_failure (Server, `Invalid_client uid) in
            send_to to_relay err t.queue;
            `Continue
        | Some (server, _client_identity) -> (
            match Spoke.server_finalize ~server client_validator with
            | Ok sk ->
                send_to (to_client ~uid) Agreement t.queue;
                t.shared_keys <- Some (uid, sk);
                `Continue
            | Error err ->
                Hashtbl.remove t.clients uid;
                send_to (to_client ~uid) (Spoke_failure (Server, err)) t.queue;
                `Continue))
    | Peer (Client, uid), Accepted -> (
        match t.shared_keys with
        | Some (uid', sk) when uid = uid' ->
            let _, client_identity = Hashtbl.find t.clients uid in
            `Done (client_identity, Spoke.ciphers_of_secret t.secret, sk)
        | Some _ | None ->
            let err = Relay_failure (Server, `No_handshake_with uid) in
            send_to to_relay err t.queue;
            `Continue)
    | Peer (Client, uid), Refused -> (
        match (t.shared_keys, Hashtbl.find_opt t.clients uid) with
        | Some (uid', _sk), Some _ when uid = uid' ->
            Hashtbl.remove t.clients uid;
            `Continue
        | Some _, _ | None, _ ->
            let err = Relay_failure (Server, `No_handshake_with uid) in
            send_to to_relay err t.queue;
            `Continue)
    | Relay, Closed (Peer (Client, client_uid)) ->
        Hashtbl.remove t.clients client_uid;
        `Continue
    | Relay, Closed Relay -> `Close
    | Relay, Timeout Server -> `Close
end

module Client = struct
  type t = {
    password : string;
    mutable identity : string;
    servers : (int, Spoke.client * string) Hashtbl.t;
    mutable shared_keys : (int * Spoke.shared_keys) option;
    mutable server_identity : string option;
    g : Random.State.t;
    queue : client send Queue.t;
    reproduce : bool;
  }

  let next_packet t =
    match Queue.pop t.queue with
    | Send_to (Peer (Server, uid), packet) -> Some (uid, packet_to_raw packet)
    | Send_to (Relay, packet) -> Some (000, packet_to_raw packet)
    | exception _ -> None

  let hello ?(reproduce = false) ~g ~identity password =
    let queue = Queue.create () in
    send_to to_relay Hello_as_a_client queue;
    {
      password;
      identity;
      shared_keys = None;
      server_identity = None;
      g;
      servers = Hashtbl.create 0x10;
      queue;
      reproduce;
    }

  let accept t =
    match t.shared_keys with
    | Some (uid, _) -> send_to (to_server ~uid) Accepted t.queue
    | None -> Fmt.invalid_arg "Impossible to accept nothing"

  let refuse t =
    match t.shared_keys with
    | Some (uid, _) -> send_to (to_server ~uid) Refused t.queue
    | None -> Fmt.invalid_arg "Impossible to refuse nothing"

  let process_packet :
      type a.
      t ->
      (a, client) src ->
      (a, client) packet ->
      [> `Continue
      | `Agreement of string
      | `Identity of string
      | `Done of string * (Spoke.cipher * Spoke.cipher) * Spoke.shared_keys
      | `Close ] =
   fun t source packet ->
    Log.debug (fun m ->
        m "Process a new packet from %04x: %a" (uid_of_src source) pp_raw
          (packet_to_raw packet));
    match (source, packet) with
    | Relay, Spoke_failure _ -> .
    | Peer (Server, uid), Spoke_failure (Server, err) ->
        (match Hashtbl.find_opt t.servers uid with
        | Some _ ->
            Log.debug (fun m -> m "[%04x]: %a" uid Spoke.pp_error err);
            Hashtbl.remove t.servers uid
        | None -> Log.warn (fun m -> m "[%04x] does not exist" uid));
        `Continue
    | Relay, Client_identity identity ->
        t.identity <- identity;
        `Identity identity
    | Relay, New_server { uid; public; identity } -> (
        Log.debug (fun m ->
            m "A new server (%04x, %s) is available." uid identity);
        match
          Spoke.hello ~g:t.g ~public:(Spoke.public_to_string public) t.password
        with
        | Ok (state, _X) ->
            Hashtbl.add t.servers uid (state, identity);
            let packet = X_and_client_identity { _X; identity = t.identity } in
            send_to (to_server ~uid) packet t.queue;
            `Continue
        | Error err ->
            send_to (to_server ~uid) (Spoke_failure (Client, err)) t.queue;
            `Continue)
    | Peer (Server, uid), Agreement -> (
        match t.shared_keys with
        | Some (uid', _sk) when uid = uid' ->
            let _, server_identity = Hashtbl.find t.servers uid in
            Log.debug (fun m ->
                m "An agreement was found with %s" server_identity);
            t.server_identity <- Some server_identity;
            `Agreement server_identity
        | Some _ | None ->
            Log.err (fun m ->
                m
                  "An agreement is expected but unique ID mismatches or no \
                   shared keys are available");
            let err = Relay_failure (Client, `No_handshake_with uid) in
            send_to to_relay err t.queue;
            `Continue)
    | Relay, Done -> (
        match (t.shared_keys, t.server_identity) with
        | Some (uid, sk), Some server_identity ->
            let client, _ = Hashtbl.find t.servers uid in
            Log.debug (fun m -> m "Agreement completed with %s" server_identity);
            `Done (server_identity, Spoke.ciphers_of_client client, sk)
        | Some _, None | None, Some _ | None, None ->
            let err = Relay_failure (Client, `No_agreement) in
            send_to to_relay err t.queue;
            `Continue)
    | Relay, Closed (Peer (Server, server_uid)) ->
        Hashtbl.remove t.servers server_uid;
        `Continue
    | Relay, Closed Relay -> `Close
    | Relay, Timeout Client -> `Close
    | Peer (Server, uid), Y_and_server_validator { _Y; server_validator } -> (
        match Hashtbl.find_opt t.servers uid with
        | None ->
            let err = Relay_failure (Client, `Invalid_server uid) in
            send_to to_relay err t.queue;
            `Continue
        | Some (client, identity) -> (
            let identities =
              match t.reproduce with
              | true -> ("foo", "bar")
              | false -> (t.identity, identity)
            in
            match
              Spoke.client_compute ~client ~identity:identities _Y
                server_validator
            with
            | Ok (sk, client_validator) ->
                t.shared_keys <- Some (uid, sk);
                let packet = Client_validator client_validator in
                send_to (to_server ~uid) packet t.queue;
                `Continue
            | Error err ->
                Hashtbl.remove t.servers uid;
                send_to (to_server ~uid) (Spoke_failure (Client, err)) t.queue;
                `Continue))
end

module Relay = struct
  module Set = Set.Make (Int)

  type t = {
    clients : (int, string * Set.t) Hashtbl.t;
    servers : (int, string * Spoke.public * Set.t) Hashtbl.t;
    uids : (string, [ `Client of int | `Server of int ]) Hashtbl.t;
    identities : string array;
    gen : unit -> int;
    queue : relay send Queue.t;
  }

  let pp ppf t =
    Fmt.pf ppf
      "@[<hov>{ active-servers:%d;@ active-clients:%d;@ packets:%d;@ }@]"
      (Hashtbl.length t.clients) (Hashtbl.length t.servers)
      (Queue.length t.queue)

  let make () =
    let gen =
      let v = ref 1 in
      fun () ->
        let ret = !v in
        incr v;
        if !v > 0xffff then v := 1;
        ret
    in
    {
      clients = Hashtbl.create 0x100;
      servers = Hashtbl.create 0x100;
      uids = Hashtbl.create 0x100;
      identities = Array.make 0xffff "\xde\xad\xbe\xef";
      gen;
      queue = Queue.create ();
    }

  let rec next_packet t =
    match Queue.take_opt t.queue with
    | None -> None
    | Some packet -> (
        match packet with
        | Respond (Peer (_, uid), packet) ->
            let identity = t.identities.(uid) in
            (match packet with
            | Done ->
                Hashtbl.remove t.uids identity;
                Hashtbl.remove t.clients uid
            | _ -> ());
            Some (identity, 00, packet_to_raw packet)
        | Transmit (Peer (_, from_uid), Peer (_, to_uid), packet) ->
            let identity = t.identities.(to_uid) in
            (match packet with
            | Accepted ->
                Hashtbl.remove t.uids identity;
                Hashtbl.remove t.servers to_uid
            | _ -> ());
            Some (identity, from_uid, packet_to_raw packet)
        | Send_to (Relay, Closed _)
        | Respond (Relay, Closed _)
        | Transmit (Relay, Relay, Closed _) ->
            next_packet t
        | Send_to (Relay, _) -> .
        | Send_to (Peer _, _) -> .
        | Respond (Relay, Timeout _) -> .
        | Respond (Relay, Relay_failure _) -> .
        | Respond (Relay, Spoke_failure _) -> .
        | Respond (Peer _, _) -> .
        | Transmit (Relay, Relay, Relay_failure _) -> .
        | Transmit (Relay, Relay, Spoke_failure _) -> .
        | Transmit (Peer _, Relay, _) -> .
        | Transmit (Relay, Peer _, _) -> .
        | Transmit (Relay, Relay, Timeout _) -> .)

  type dst_rel =
    | Relay_packet : ('a, 'b) dst * ('a, 'b) packet -> dst_rel
    | Invalid_packet of int * raw

  let dst_and_packet ~identity t uid packet =
    match (uid, packet) with
    | 00, `Hello_as_a_server public ->
        Relay_packet (Relay, Hello_as_a_server { public })
    | 00, `Hello_as_a_client -> Relay_packet (Relay, Hello_as_a_client)
    | 00, `Relay_failure err -> (
        match Hashtbl.find_opt t.uids identity with
        | Some (`Server _uid) ->
            Relay_packet (Relay, Relay_failure (Server, err))
        | Some (`Client _uid) ->
            Relay_packet (Relay, Relay_failure (Client, err))
        | _ -> Invalid_packet (uid, packet))
    | uid, `Client_validator validator ->
        Relay_packet (Peer (Server, uid), Client_validator validator)
    | uid, `Y_and_server_validator (_Y, server_validator) ->
        Relay_packet
          (Peer (Client, uid), Y_and_server_validator { _Y; server_validator })
    | uid, `X_and_client_identity (_X, identity) ->
        Relay_packet (Peer (Server, uid), X_and_client_identity { _X; identity })
    | uid, `Agreement -> Relay_packet (Peer (Client, uid), Agreement)
    | uid, `Accepted -> Relay_packet (Peer (Server, uid), Accepted)
    | uid, `Refused -> Relay_packet (Peer (Server, uid), Refused)
    | uid, `Spoke_failure err -> (
        match Hashtbl.find_opt t.uids identity with
        | Some (`Server _) ->
            Relay_packet (Peer (Client, uid), Spoke_failure (Server, err))
        | Some (`Client _) ->
            Relay_packet (Peer (Server, uid), Spoke_failure (Client, err))
        | _ -> Invalid_packet (uid, packet))
    | uid, packet -> Invalid_packet (uid, packet)

  let all_clients t =
    Hashtbl.fold (fun uid _ -> Set.add uid) t.clients Set.empty

  let all_servers t =
    Hashtbl.fold (fun uid _ -> Set.add uid) t.servers Set.empty

  let remove_server_candidate_from_clients t uid =
    let active_clients = ref Set.empty in
    Hashtbl.filter_map_inplace
      (fun client_uid (client_identity, servers) ->
        if Set.mem uid servers then
          active_clients := Set.add client_uid !active_clients;
        Some (client_identity, Set.remove uid servers))
      t.clients;
    !active_clients

  let remove_client_candidate_from_servers t uid =
    let active_servers = ref Set.empty in
    Hashtbl.filter_map_inplace
      (fun server_uid (server_identity, public, clients) ->
        if Set.mem uid clients then
          active_servers := Set.add server_uid !active_servers;
        Some (server_identity, public, Set.remove uid clients))
      t.servers;
    !active_servers

  let exists ~identity t = Hashtbl.mem t.uids identity

  let delete ~identity t =
    match Hashtbl.find_opt t.uids identity with
    | Some (`Server uid) ->
        Log.debug (fun m -> m "Delete the server %04x" uid);
        Hashtbl.remove t.servers uid;
        Hashtbl.remove t.uids identity;
        let clients = remove_server_candidate_from_clients t uid in
        respond (to_server ~uid) (Timeout Server) t.queue;
        let packet = Closed (Peer (Server, uid)) in
        Set.iter (fun uid -> respond (to_client ~uid) packet t.queue) clients
    | Some (`Client uid) ->
        Log.debug (fun m -> m "Delete the client %04x" uid);
        Hashtbl.remove t.clients uid;
        Hashtbl.remove t.uids identity;
        let servers = remove_client_candidate_from_servers t uid in
        respond (to_client ~uid) (Timeout Client) t.queue;
        let packet = Closed (Peer (Client, uid)) in
        Set.iter (fun uid -> respond (to_server ~uid) packet t.queue) servers
    | None -> ()

  let pp_error ppf = function
    | `Invalid_server uid -> Fmt.pf ppf "Invalid server uid %04x" uid
    | `Invalid_client uid -> Fmt.pf ppf "Invalid client uid %04x" uid
    | `No_handshake_with uid -> Fmt.pf ppf "No handshake with %04x" uid
    | `No_agreement -> Fmt.string ppf "No agreement"

  let remove_candidates :
      type a b. src:(a, b) src -> dst:(a, b) dst -> t -> unit =
   fun ~src ~dst t ->
    match (src, dst) with
    | Peer (Server, s), Peer (Client, c) | Peer (Client, c), Peer (Server, s)
      -> (
        match (Hashtbl.find_opt t.servers s, Hashtbl.find_opt t.clients c) with
        | Some (s_identity, public, clients), Some (c_identity, servers) ->
            Hashtbl.replace t.servers s
              (s_identity, public, Set.remove c clients);
            Hashtbl.replace t.clients c (c_identity, Set.remove s servers)
        | Some (s_identity, public, clients), None ->
            Hashtbl.replace t.servers s
              (s_identity, public, Set.remove c clients)
        | None, Some (c_identity, servers) ->
            Hashtbl.replace t.clients c (c_identity, Set.remove s servers)
        | None, None -> ())
    | Relay, Peer _ -> .
    | Peer _, Relay -> .
    | Relay, Relay -> ()

  let process_packet :
      type a b c.
      t ->
      identity:string ->
      (a, b) src ->
      (a, b, c) transmit ->
      (a, c) dst ->
      (a, c) packet ->
      [> `Continue | `Agreement of string * string ] =
   fun t ~identity src link dst packet ->
    match (src, link, dst, packet) with
    | Peer (_, uid), _, Relay, Relay_failure (_, err) ->
        Log.err (fun m -> m "[%04x]: %a" uid pp_error err);
        `Continue
    (* XXX(dinosaure): impossible cases:
       - Send a [spoke] error to the relay
       - Send a [relay] error to a peer
       - receive a relay error from the relay (I'M THE RELAY)
       - a gently hello directly to a {client,server}
    *)
    | _, _, Relay, Spoke_failure _ -> .
    | _, _, Peer _, Relay_failure _ -> .
    | Relay, Blind, Relay, Relay_failure _ -> .
    | Peer _, From_client, Peer _, _ -> .
    | Peer _, From_server, Peer _, _ -> .
    | Peer (Client, uid), From_client, Relay, Hello_as_a_client ->
        Log.debug (fun m -> m "New client into the pool.");
        let uid =
          match uid with
          | 0 ->
              let uid = t.gen () in
              Hashtbl.replace t.uids identity (`Client uid);
              t.identities.(uid) <- identity;
              Hashtbl.add t.clients uid (identity, all_servers t);
              uid
          | uid ->
              Log.warn (fun m ->
                  m "The incoming client %s already as an unique ID: %04x"
                    identity uid);
              uid
        in
        respond (to_client ~uid) (Client_identity identity) t.queue;
        (* broadcast available servers to **the** client *)
        Hashtbl.filter_map_inplace
          (fun server_uid (identity, public, clients) ->
            let clients = Set.add uid clients in
            let packet = New_server { uid = server_uid; public; identity } in
            respond (to_client ~uid) packet t.queue;
            Some (identity, public, clients))
          t.servers;
        `Continue
    | Peer (Server, uid), From_server, Relay, Hello_as_a_server { public } ->
        Log.debug (fun m -> m "New server into the pool.");
        let uid =
          match uid with
          | 0 ->
              let uid = t.gen () in
              Hashtbl.replace t.uids identity (`Server uid);
              t.identities.(uid) <- identity;
              Hashtbl.add t.servers uid (identity, public, all_clients t);
              uid
          | uid ->
              Log.warn (fun m ->
                  m "The incoming server %s already as an unique ID: %04x"
                    identity uid);
              uid
        in
        respond (to_server ~uid) (Server_identity identity) t.queue;
        (* broadcast **the** server to available clients *)
        Hashtbl.filter_map_inplace
          (fun client_uid (client_identity, servers) ->
            let servers = Set.add uid servers in
            let packet = New_server { uid; public; identity } in
            respond (to_client ~uid:client_uid) packet t.queue;
            Some (client_identity, servers))
          t.clients;
        `Continue
    | ( (Peer (Client, from_uid) as src),
        Blind,
        (Peer (Server, to_uid) as dst),
        (Spoke_failure _ as packet) ) ->
        Log.debug (fun m ->
            m "Handshake error between [%04x] & [%04x]" from_uid to_uid);
        remove_candidates ~src ~dst t;
        transmit ~src ~dst packet t.queue;
        `Continue
    | ( (Peer (Server, from_uid) as src),
        Blind,
        (Peer (Client, to_uid) as dst),
        (Spoke_failure _ as packet) ) ->
        Log.debug (fun m ->
            m "Handshake error between [%04x] & [%04x]" from_uid to_uid);
        remove_candidates ~src ~dst t;
        transmit ~src ~dst packet t.queue;
        `Continue
    | Peer (Client, from_uid), Blind, Peer (Server, to_uid), Accepted -> (
        match
          ( Hashtbl.find_opt t.clients from_uid,
            Hashtbl.find_opt t.servers to_uid )
        with
        | Some (client_identity, servers), Some (server_identity, _, clients) ->
            let inter = Set.elements (Set.inter servers clients) in
            Log.debug (fun m ->
                m "[%04x] agreed with [%04x] to exchange" from_uid to_uid);
            Log.debug (fun m ->
                m "Intersection: @[<hov>%a@]" Fmt.(Dump.list int) inter);
            (* clean hashtbls *)
            Hashtbl.remove t.clients from_uid;
            Hashtbl.remove t.servers to_uid;
            transmit ~src ~dst packet t.queue;
            respond (to_client ~uid:from_uid) Done t.queue;
            `Agreement (client_identity, server_identity)
        | None, Some _ | Some _, None | None, None ->
            Log.err (fun m ->
                m "An agreement exists between a peer and nothing.");
            (* transmit ~src ~dst packet t.queue ; *)
            `Continue)
    | ( (Peer (Client, from_uid) as src),
        Blind,
        (Peer (Server, to_uid) as dst),
        Refused ) ->
        Log.debug (fun m ->
            m
              "[%04x] refused the transfer with [%04x] even if they found an \
               agreement."
              from_uid to_uid);
        remove_candidates ~src ~dst t;
        `Continue
    | src, Blind, dst, packet ->
        transmit ~src ~dst packet t.queue;
        `Continue
    (* XXX(dinosaure): impossible cases:
       - a "blind" transmission between a peer and the relay
       - a packet from the relay
       - a gently hello directly to a {client,server}

       OCaml is not smart enough to deduct types of inner values.
    *)
    | Peer _, Blind, Relay, _ -> .
    | Relay, Blind, Peer _, _ -> .
    | Peer (Client, _), From_client, Peer _, Hello_as_a_client -> .
    | Peer (Server, _), From_server, Peer _, Hello_as_a_server _ -> .

  type exists =
    | Exists : ('f, 't) peer * ('f, 't) src -> exists
    | None : exists

  let process_packet :
      type a b.
      t ->
      identity:string ->
      (a, b) dst ->
      (a, b) packet ->
      [> `Continue | `Agreement of string * string ] =
   fun t ~identity dst packet ->
    let src =
      match Hashtbl.find_opt t.uids identity with
      | Some (`Client uid) -> Exists (Client, Peer (Client, uid))
      | Some (`Server uid) -> Exists (Server, Peer (Server, uid))
      | None -> None
    in
    match (src, dst, packet) with
    (* XXX(dinosaure): impossible cases:
       - A peer is a relay
       - Send a [spoke] error to the relay
    *)
    | Exists (_, Relay), _, _ -> .
    | _, Relay, Spoke_failure _ -> .
    | Exists (_, Peer _), Relay, Timeout _ -> .
    | None, Relay, Timeout _ -> .
    | Exists (Client, src), Relay, Relay_failure (Client, _) ->
        process_packet t ~identity src From_client Relay packet
    | Exists (Server, src), Relay, Relay_failure (Server, _) ->
        process_packet t ~identity src From_server Relay packet
    | Exists (Client, src), (Peer (Server, _) as dst), packet ->
        process_packet t ~identity src Blind dst packet
    | Exists (Server, src), (Peer (Client, _) as dst), packet ->
        process_packet t ~identity src Blind dst packet
    | None, Relay, Hello_as_a_client ->
        let src : _ src = Peer (Client, 0) in
        process_packet t ~identity src From_client dst Hello_as_a_client
    | None, Relay, (Hello_as_a_server _ as packet) ->
        let src : _ src = Peer (Server, 0) in
        process_packet t ~identity src From_server dst packet
    (* A registered client, wants to say (again) hello. *)
    | Exists (Client, src), Relay, Hello_as_a_client ->
        process_packet t ~identity src From_client dst packet
    (* A registered server, wants to say (again) hello. *)
    | Exists (Server, src), Relay, Hello_as_a_server _ ->
        process_packet t ~identity src From_server dst packet
    | Exists (_, Peer _), Relay, Closed (Peer _) -> .
    (* XXX(dinosaure): it's impossible to craft on the [Client]/[Server] side
       this specific packet: [send_to to_relay (Closed Relay]. If we receive
       it, it comes from something else than our implementation! Let's
       [`Continue]. *)
    | Exists (_, Peer _), Relay, Closed Relay -> `Continue
    | None, Relay, Closed _ -> `Continue
    (* A client wants to communicate to a client. *)
    | Exists (_, Peer (Client, src)), Peer (Client, dst), _ ->
        Log.err (fun m ->
            m
              "An existing client (%04x) wants to communicate with another \
               client (%04x)"
              src dst);
        `Continue
    (* A server wants to communicate to a server. *)
    | Exists (_, Peer (Server, src)), Peer (Server, dst), _ ->
        Log.err (fun m ->
            m
              "An existing server (%04x) wants to communicate with another \
               server (%04x)"
              src dst);
        `Continue
    (* An unregistered peer wants to communicate to a client. *)
    | None, Peer (Client, dst), _ ->
        Log.err (fun m ->
            m "An unregistered peer wants to communicate with a client (%04x)"
              dst);
        `Continue
    (* An unregistered peer wants to communicate to a server. *)
    | None, Peer (Server, dst), _ ->
        Log.err (fun m ->
            m "An unregistered peer wants to communicate with a server (%04x)"
              dst);
        `Continue
    (* A source identified as a client wants to send an error
       to the relay as a server. *)
    | Exists (Client, _), Relay, Relay_failure (Server, _) -> `Continue
    (* A source identified as a server wants to send an error
       to the relay as a client. *)
    | Exists (Server, _), Relay, Relay_failure (Client, _) -> `Continue
    (* An unregistered peer wants to send an error to the relay. *)
    | None, Relay, Relay_failure _ -> `Continue
    | Exists (Client, src), Relay, Hello_as_a_server _ ->
        let src = uid_of_src src in
        Log.err (fun m ->
            m "An already registered client (%S:%04x) wants to say hello."
              identity src);
        `Continue
    | Exists (Server, src), Relay, Hello_as_a_client ->
        let src = uid_of_src src in
        Log.err (fun m ->
            m "An already registered server (%S:%04x) wants to say hello."
              identity src);
        `Continue
end
