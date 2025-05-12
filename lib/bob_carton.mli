val delta :
  reporter:(int -> unit Fiber.t) ->
  load:(Carton.Uid.t -> 'meta -> Carton.Value.t Fiber.t) ->
  'meta Cartonnage.Entry.t Bob_stream.stream ->
  'meta Cartonnage.Target.t Bob_stream.stream

val pack :
  reporter:(unit -> unit Fiber.t) ->
  ?level:int ->
  length:int ->
  (Carton.Uid.t -> 'meta -> Carton.Value.t Fiber.t) ->
  ('meta Cartonnage.Target.t, Bstr.t) Bob_stream.flow

type entry = Base of Carton.Kind.t | Ofs of int | Ref of Digestif.SHA1.t
type elt = { offset : int; entry : entry; queue : string Queue.t }

val unpack :
  unit -> (Bstr.t, [ `Elt of elt | `End of Digestif.SHA1.t ]) Bob_stream.flow

val verify :
  on:(Carton.Value.t -> Carton.Uid.t -> unit) ->
  'fd Carton.t ->
  Carton.oracle ->
  Carton.status array ->
  unit Fiber.t
