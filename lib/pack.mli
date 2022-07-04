module Temp = Temp

type pattern = (string -> string, Format.formatter, unit, string) format4
type store

val store : Fpath.t -> Digestif.SHA256.t list * store

val deltify :
  reporter:(int -> unit Fiber.t) ->
  store ->
  Digestif.SHA256.t list ->
  Digestif.SHA256.t Carton.Enc.q array Fiber.t

val make :
  ?tmp:pattern ->
  ?g:Random.State.t ->
  reporter:(unit -> unit Fiber.t) ->
  store ->
  Digestif.SHA256.t Carton.Enc.q array ->
  Fpath.t Fiber.t
