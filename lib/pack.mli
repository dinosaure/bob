module Temp = Temp

type pattern = (string -> string, Format.formatter, unit, string) format4
type store

val store : Fpath.t -> Digestif.SHA256.t list * store

val make :
  ?tmp:pattern ->
  ?g:Random.State.t ->
  ?reporter:(int -> unit Fiber.t) ->
  store ->
  Digestif.SHA256.t list ->
  unit Fiber.t
