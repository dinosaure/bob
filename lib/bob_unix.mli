type error =
  [ `Connection_closed_by_relay
  | Bob.Protocol.error ]

val pp_error : error Fmt.t

val server : Unix.file_descr -> g:Random.State.t -> secret:Spoke.secret ->
  (Spoke.shared_keys, error) result Fiber.t

val client : Unix.file_descr -> choose:[ `Accept | `Refuse ] Fiber.Ivar.t ->
  g:Random.State.t -> password:string ->
  ([ `Accepted_with of Spoke.shared_keys
   | `Refused ], error) result Fiber.t

val serve_when_ready :
  ?stop:unit Fiber.Ivar.t ->
  handler:(Unix.file_descr -> Unix.sockaddr -> unit Fiber.t) ->
  Unix.file_descr -> unit Fiber.t

val relay : Unix.file_descr -> stop:unit Fiber.Ivar.t -> unit Fiber.t
