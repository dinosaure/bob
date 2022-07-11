type store

val store : Fpath.t -> (Digestif.SHA256.t Stream.stream * store) Fiber.t
(** [store path] aggregates all files and directories from the given path. It
    returns a [store] and a stream of hash of these objects. *)

val length : store -> int
(** [length store] returns the number of {i objects} into the given store. *)

val deltify :
  reporter:(int -> unit Fiber.t) ->
  ?compression:bool ->
  store ->
  Digestif.SHA256.t Stream.stream ->
  Digestif.SHA256.t Carton.Enc.q Stream.stream Fiber.t
(** [deltify ~reporter ?compression store hashes] tries to compress with patch
    objects together. If [compression] is true (default), it calculates
    the patch between the objects and chooses the best. Otherwise, it
    generates a stream of objects uncompressed between them. *)

val make :
  ?level:int ->
  reporter:(unit -> unit Fiber.t) ->
  store ->
  (Digestif.SHA256.t Carton.Enc.q, Stdbob.bigstring) Stream.flow
(** [make ?level ~reporter store] returns a {i flow} which transform a list
   of objects into a series of [string]. [level] lets the user to choose the
   [zlib] level compression (between [0] and [9]). *)

type status

val first_pass :
  reporter:(int -> unit Fiber.t) ->
  Stdbob.bigstring Stream.source ->
  (int64
  * status
  * [ `Base of Carton.Dec.weight
    | `Ofs of int * Carton.Dec.weight * Carton.Dec.weight * Carton.Dec.weight
    | `Ref of
      Digestif.SHA256.t
      * Carton.Dec.weight
      * Carton.Dec.weight
      * Carton.Dec.weight ])
  Stream.source
