module Protocol = Protocol
module State = State
module Crypto = Crypto

module Server : sig
  type t

  val hello : ?reproduce:bool -> g:Random.State.t -> Spoke.secret -> t

  val receive :
    t ->
    [ `End | `Data of string * int * int ] ->
    [> `Continue
    | `Read
    | `Close
    | `Done of string * (Spoke.cipher * Spoke.cipher) * Spoke.shared_keys
    | `Agreement of string
    | `Error of Protocol.error ]

  val send : t -> [> `Continue | `Write of string | `Error of Protocol.error ]
end

module Client : sig
  type t

  val make :
    ?reproduce:bool -> g:Random.State.t -> identity:string -> string -> t

  val receive :
    t ->
    [ `End | `Data of string * int * int ] ->
    [> `Continue
    | `Read
    | `Close
    | `Done of string * (Spoke.cipher * Spoke.cipher) * Spoke.shared_keys
    | `Agreement of string
    | `Error of Protocol.error ]

  val send : t -> [> `Continue | `Write of string | `Error of Protocol.error ]
  val agreement : t -> [ `Accept | `Refuse ] -> unit
end

module Relay : sig
  type t

  val make : unit -> t
  val new_peer : t -> identity:string -> unit
  val rem_peer : t -> identity:string -> unit
  val exists : t -> identity:string -> bool
  val pp : t Fmt.t

  val receive_from :
    t ->
    identity:string ->
    [ `End | `Data of string * int * int ] ->
    [> `Continue | `Close | `Agreement of string * string | `Read ]

  val send_to :
    t -> [> `Continue | `Close of string | `Write of string * string ]
end

module Secured : sig
  type t

  val make : unit -> t
  val add_peers : t -> string -> string -> unit
  val rem_peers : t -> string -> string -> unit

  type reader =
    [ `Data of string * int * int | `End ] ->
    [ `Continue of 'k | `Invalid_peer | `Peer of string * string ]
    as
    'k

  val reader : t -> reader
end
