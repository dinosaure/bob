val delta :
  reporter:(int -> unit Fiber.t) ->
  load:(Carton.Uid.t -> 'meta -> Carton.Value.t Fiber.t) ->
  'meta Cartonnage.Entry.t Stream.stream ->
  'meta Cartonnage.Target.t Stream.stream

val pack :
  reporter:(unit -> unit Fiber.t) ->
  ?level:int ->
  length:int ->
  (Carton.Uid.t -> 'meta -> Carton.Value.t Fiber.t) ->
  ('meta Cartonnage.Target.t, Stdbob.bigstring) Stream.flow

val verify :
  on:(Carton.Value.t -> Carton.Uid.t -> unit) ->
  'fd Carton.t ->
  Carton.oracle ->
  Carton.status array ->
  unit Fiber.t
