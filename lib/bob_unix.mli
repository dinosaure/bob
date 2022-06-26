(* {1:Bob implementation with Unix module.}

   This module provides a specialization of {!module:Bob} with the [Unix]
   module (in others words, this module is incompatible with MirageOS). It
   provides a client, a server and a relay implementation.

   The user can execute these programs with the module {!module:Fiber}. It
   provides the essential to launch a relay, create a client or a server from
   a {!val:Unix.file_descr} {b already} bound/connected. {!val:server} and
   {!val:client} properly close the given {!val:Unix.file_descr}.
*)

module type IO = sig
  type fd

  type error
  type write_error = private [> `Closed ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t

  val of_file_descr : Unix.file_descr -> (fd, write_error) result Fiber.t
  val recv : fd -> ([ `End | `Data of string ], error) result Fiber.t
  val send : fd -> off:int -> len:int -> string -> (int, write_error) result Fiber.t
  val close : fd -> unit Fiber.t
end

val serve_when_ready :
  ?stop:unit Fiber.Ivar.t ->
  handler:(Unix.file_descr -> Unix.sockaddr -> unit Fiber.t) ->
  Unix.file_descr -> unit Fiber.t

module Make (IO : IO) : sig
  type error =
    [ `Connection_closed_by_relay
    | `Wr of IO.write_error
    | `Rd of IO.error
    | Bob.Protocol.error ]
  
  val pp_error : error Fmt.t
  
  val server :
    IO.fd -> g:Random.State.t -> secret:Spoke.secret ->
    (string * Spoke.shared_keys, error) result Fiber.t
  (** [server socket ~g ~secret] tries to find {i via} a relay (represented by
      the given [socket]), a peer which shares the same password as you. *)
  
  val client :
    IO.fd -> choose:(string -> [ `Accept | `Refuse ] Fiber.t) ->
    g:Random.State.t -> password:string ->
    (string * Spoke.shared_keys, error) result Fiber.t
  (** [client socket ~choose ~g ~password] tries to find {i via} a relay
      (represented by the given [socket]), a peer which shares the same password
      as you. When the client found it, the user must fill [choose] to accept
      or refuse the peer found. *)
  
 
  val relay :
    ?timeout:float -> Unix.file_descr -> stop:unit Fiber.Ivar.t -> unit Fiber.t
  (** [relay ?timeout socket ~stop] launch a relay which can accept connections
      from [socket]. The user can specify a [timeout] value (how long the relay
      can keep an active connection with a peer). If [stop] is filled, the relay
      terminates. *)
end
