module Protocol = Protocol
module State = State

module Server : sig
  type t

  val hello : g:Random.State.t -> secret:Spoke.secret -> t

  val receive : t -> [ `End | `Data of (string * int * int) ] ->
    [> `Continue | `Read | `Close
    |  `Done      of string * Spoke.shared_keys
    |  `Agreement of string
    |  `Error     of Protocol.error ]

  val send : t ->
    [> `Continue
    |  `Write of string
    |  `Error of Protocol.error ]
end

module Client : sig
  type t

  val make : g:Random.State.t -> password:string -> identity:string -> t

  val receive : t -> [ `End | `Data of (string * int * int) ] ->
    [> `Continue | `Read | `Close
    |  `Done      of string * Spoke.shared_keys
    |  `Agreement of string
    |  `Error     of Protocol.error ]

  val send : t ->
    [> `Continue
    |  `Write of string
    |  `Error of Protocol.error ]

  val agreement : t -> [ `Accept | `Refuse ] -> unit
end

module Relay : sig
  type t

  val make : unit -> t
  val new_peer : t -> identity:string -> unit
  val rem_peer : t -> identity:string -> unit
  val exists : t -> identity:string -> bool
  val pp : t Fmt.t

  val receive_from : t -> identity:string ->
    [ `End | `Data of (string * int * int) ] ->
    [> `Continue | `Close | `Agreement of string * string | `Read ]

  val send_to : t ->
    [> `Continue 
    |  `Close of string
    |  `Write of string * string ]
end
