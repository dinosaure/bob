type t

type error =
  [ `Closed
  | `Unix of Unix.error
  | `Alert of Tls.Packet.alert_type
  | `Failure of Tls.Engine.failure ]

val pp_error : error Fmt.t

exception Tls of error

val read : t -> Cstruct.t -> (int, error) result Fiber.t
val writev : t -> Cstruct.t list -> unit Fiber.t
val write : t -> Cstruct.t -> unit Fiber.t
val close : t -> unit Fiber.t

val client_of_file_descr :
  Tls.Config.client ->
  ?host:[ `host ] Domain_name.t ->
  Unix.file_descr ->
  (t, error) result Fiber.t
