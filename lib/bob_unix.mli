(* {1:Bob implementation with Unix module.}

   This module provides a specialization of {!module:Bob} with the [Unix]
   module (in others words, this module is incompatible with MirageOS). It
   provides a client, a server and a relay implementation.

   The user can execute these programs with the module {!module:Fiber}. It
   provides the essential to launch a relay, create a client or a server from
   a {!val:Unix.file_descr} {b already} bound/connected. {!val:server} and
   {!val:client} properly close the given {!val:Unix.file_descr}.
*)

type error =
  [ `Connection_closed_by_relay
  | `Write of Unix.error
  | Bob.Protocol.error ]

val pp_error : error Fmt.t

val server :
  Unix.file_descr -> g:Random.State.t -> secret:Spoke.secret ->
  (string * Spoke.shared_keys, error) result Fiber.t
(** [server socket ~g ~secret] tries to find {i via} a relay (represented by
    the given [socket]), a peer which shares the same password as you. *)

val client :
  Unix.file_descr -> choose:(string -> [ `Accept | `Refuse ] Fiber.t) ->
  g:Random.State.t -> password:string ->
  ([ `Accepted_with of string * Spoke.shared_keys
   | `Refused ], error) result Fiber.t
(** [client socket ~choose ~g ~password] tries to find {i via} a relay
    (represented by the given [socket]), a peer which shares the same password
    as you. When the client found it, the user must fill [choose] to accept
    or refuse the peer found. *)

val serve_when_ready :
  ?stop:unit Fiber.Ivar.t ->
  handler:(Unix.file_descr -> Unix.sockaddr -> unit Fiber.t) ->
  Unix.file_descr -> unit Fiber.t

val relay :
  ?timeout:float -> Unix.file_descr -> stop:unit Fiber.Ivar.t -> unit Fiber.t
(** [relay ?timeout socket ~stop] launch a relay which can accept connections
    from [socket]. The user can specify a [timeout] value (how long the relay
    can keep an active connection with a peer). If [stop] is filled, the relay
    terminates. *)
