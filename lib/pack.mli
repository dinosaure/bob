type store

val store : Fpath.t -> (Digestif.SHA1.t Stream.stream * store) Fiber.t
(** [store path] aggregates all files and directories from the given path. It
    returns a [store] and a stream of hash of these objects. *)

val length : store -> int
(** [length store] returns the number of {i objects} into the given store. *)

val deltify :
  reporter:(int -> unit Fiber.t) ->
  ?compression:bool ->
  store ->
  Digestif.SHA1.t Stream.stream ->
  Digestif.SHA1.t Carton.Enc.q Stream.stream Fiber.t
(** [deltify ~reporter ?compression store hashes] tries to compress with patch
    objects together. If [compression] is true (default), it calculates
    the patch between the objects and chooses the best. Otherwise, it
    generates a stream of objects uncompressed between them. *)

val make :
  ?level:int ->
  reporter:(unit -> unit Fiber.t) ->
  store ->
  (Digestif.SHA1.t Carton.Enc.q, Stdbob.bigstring) Stream.flow
(** [make ?level ~reporter store] returns a {i flow} which transform a list
   of objects into a series of [string]. [level] lets the user to choose the
   [zlib] level compression (between [0] and [9]). *)

val make_one :
  ?level:int ->
  reporter:(int -> unit Fiber.t) ->
  finalise:(unit -> unit) ->
  Fpath.t ->
  (Stdbob.bigstring Stream.stream, [> `Msg of string ]) result Fiber.t
(** [make ?level ~reporter path] returns the path of the generated PACK file
   of the given file. [level] lets the user to choose the [zlib] level compression
   (between [0] and [9]). *)

type status

type entry =
  int64
  * status
  * [ `Base of Carton.Dec.weight
    | `Ofs of int * Carton.Dec.weight * Carton.Dec.weight * Carton.Dec.weight
    | `Ref of
      Digestif.SHA1.t
      * Carton.Dec.weight
      * Carton.Dec.weight
      * Carton.Dec.weight ]

val is_base : status -> bool
val is_resolved : status -> bool
val offset_of_status : status -> int64
val kind_of_status : status -> [ `A | `B | `C | `D ]

val first_pass :
  reporter:(int -> unit Fiber.t) ->
  Stdbob.bigstring Stream.source ->
  entry Stream.source

val collect :
  entry Stream.source ->
  (status array * Digestif.SHA1.t Carton.Dec.oracle) Fiber.t

val verify :
  ?reporter:(unit -> unit) ->
  oracle:Digestif.SHA1.t Carton.Dec.oracle ->
  Fpath.t ->
  status array ->
  unit Fiber.t

val extract_file : file:status -> name:status -> Fpath.t -> unit Fiber.t
