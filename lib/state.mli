type client and server and relay

type ('f, 't) src and ('f, 't) dst

type ('f, 't) packet

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
  | `Relay_failure of [ `Invalid_client of int
                      | `Invalid_server of int
                      | `No_handshake_with of int ]
  | `Spoke_failure of Spoke.error ]

type src_rel =
  | Server_packet : ('a, server) src * ('a, server) packet -> src_rel
  | Client_packet : ('a, client) src * ('a, client) packet -> src_rel
  | Invalid_packet of int * raw

val src_and_packet :
  peer:[ `Server | `Client ] -> int -> raw -> src_rel

module Server : sig
  type t

  val hello : g:Random.State.t -> secret:Spoke.secret -> t
  val process_packet : t -> ('a, server) src -> ('a, server) packet ->
    [ `Continue | `Done of Spoke.shared_keys | `Close ]
  val next_packet : t -> (int * raw) option
end

module Client : sig
  type t

  val make : g:Random.State.t -> password:string -> identity:string -> t
  val accept : t -> unit
  val refuse : t -> unit
  val process_packet : t -> ('a, client) src -> ('a, client) packet ->
    [ `Continue | `Done of Spoke.shared_keys | `Close ]
  val next_packet : t -> (int * raw) option
end

module Relay : sig
  type t

  val make : unit -> t
  val pp : t Fmt.t

  type dst_rel =
    | Relay_packet : ('a, 'b) dst * ('a, 'b) packet -> dst_rel
    | Invalid_packet of int * raw

  val dst_and_packet : identity:string -> t -> int -> raw -> dst_rel

  val delete : identity:string -> t -> unit
  val process_packet : t -> identity:string ->
   ('a, 'b) dst -> ('a, 'b) packet ->
   [ `Continue | `Agreement of string * string ]
  val next_packet : t -> (string * int * raw) option
end
