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
  ?level:int ->
  reporter:(unit -> unit Fiber.t) ->
  store ->
  Digestif.SHA256.t Carton.Enc.q array ->
  Fpath.t Fiber.t

type status

val first_pass :
  in_channel ->
  reporter:(int -> unit Fiber.t) ->
  ( Digestif.SHA256.t
    * status array
    * Digestif.SHA256.t Carton.Dec.oracle
    * (int64, Optint.t) Hashtbl.t,
    [> `Msg of string ] )
  result
  Fiber.t
