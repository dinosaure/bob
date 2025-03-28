type store

val store : Bob_fpath.t -> (Carton.Uid.t Stream.stream * store) Fiber.t
(** [store path] aggregates all files and directories from the given path. It
    returns a [store] and a stream of hash of these objects. *)

val length : store -> int
(** [length store] returns the number of {i objects} into the given store. *)

val deltify :
  reporter:(int -> unit Fiber.t) ->
  ?compression:bool ->
  store ->
  Carton.Uid.t Stream.stream ->
  unit Cartonnage.Target.t Stream.stream
(** [deltify ~reporter ?compression store hashes] tries to compress with patch
    objects together. If [compression] is true (default), it calculates the
    patch between the objects and chooses the best. Otherwise, it generates a
    stream of objects uncompressed between them. *)

val make :
  ?level:int ->
  reporter:(unit -> unit Fiber.t) ->
  store ->
  (unit Cartonnage.Target.t, Stdbob.bigstring) Stream.flow
(** [make ?level ~reporter store] returns a {i flow} which transform a list of
    objects into a series of [string]. [level] lets the user to choose the
    [zlib] level compression (between [0] and [9]). *)

val make_one :
  ?level:int ->
  reporter:(int -> unit Fiber.t) ->
  finalise:(unit -> unit) ->
  Bob_fpath.t ->
  (Stdbob.bigstring Stream.stream, [> `Msg of string ]) result Fiber.t
(** [make ?level ~reporter path] returns the path of the generated PACK file of
    the given file. [level] lets the user to choose the [zlib] level compression
    (between [0] and [9]). *)

val inflate_entry :
  reporter:(int -> unit Fiber.t) ->
  (Stdbob.bigstring, Stdbob.bigstring) Stream.flow
(** [inflate_entry ~reporter] creates a flow (usable with {!Stream.run} for
    instance) which deflates an entry (including its header). The given input (a
    {!Stream.source} or a {!Stream.stream}) must start at the beginning of the
    entry. It returns the deflated entry. An entry can be a [`Base] (and, in
    such case, you extract the entry) or a patch (and, in such case, you must
    reconstruct the entry with its source). *)

type status
type decoder = Carton.First_pass.decoder

type entry =
  int
  * status
  * [ `Base of Carton.Kind.t * Carton.Size.t
    | `Ofs of int * Carton.Size.t * Carton.Size.t * Carton.Size.t
    | `Ref of Carton.Uid.t * Carton.Size.t * Carton.Size.t * Carton.Size.t ]

type elt = [ `End of string | `Elt of entry ]

val analyse :
  ?decoder:decoder ->
  (int -> unit Fiber.t) ->
  (Stdbob.bigstring, elt * decoder option * Stdbob.bigstring * int) Stream.flow

val collect : entry Stream.source -> (status array * Carton.oracle) Fiber.t

val verify :
  ?reporter:(unit -> unit) ->
  oracle:Carton.oracle ->
  Bob_fpath.t ->
  status array ->
  unit Fiber.t

type unpacked =
  string * int * Carton.Uid.t * (Unix.file_descr * Unix.stats) Carton.t

val unpack :
  Bob_fpath.t -> status array -> (unpacked, [> `No_root ]) result Fiber.t

val create_directory :
  reporter:(int -> unit) ->
  'fd Carton.t ->
  Bob_fpath.t ->
  Carton.Uid.t ->
  'fd Carton.t Fiber.t

(*
type status
type decoder

val ctx : decoder -> Digestif.SHA1.ctx
val is_base : status -> bool
val is_resolved : status -> bool
val offset_of_status : status -> int64
val kind_of_status : status -> [ `A | `B | `C | `D ]
val uid_of_status : status -> Digestif.SHA1.t

<<<<<<< Updated upstream
val analyse :
  ?decoder:decoder ->
  (int -> unit Fiber.t) ->
  ( Stdbob.bigstring,
    [ `End of Digestif.SHA1.t | `Elt of entry ]
    * decoder option
    * Stdbob.bigstring
    * int )
  Stream.flow

val inflate_entry :
  reporter:(int -> unit Fiber.t) ->
  (Stdbob.bigstring, Stdbob.bigstring) Stream.flow
(** [inflate_entry ~reporter] creates a flow (usable with {!Stream.run} for
    instance) which deflates an entry (including its header). The given input (a
    {!Stream.source} or a {!Stream.stream}) must start at the beginning of the
    entry. It returns the deflated entry. An entry can be a [`Base] (and, in
    such case, you extract the entry) or a patch (and, in such case, you must
    reconstruct the entry with its source). *)

=======
>>>>>>> Stashed changes
val collect :
  entry Stream.source ->
  (status array * Digestif.SHA1.t Carton.Dec.oracle) Fiber.t

val verify :
  ?reporter:(unit -> unit) ->
  oracle:Digestif.SHA1.t Carton.Dec.oracle ->
  Bob_fpath.t ->
  status array ->
  unit Fiber.t

val create_directory :
  reporter:(int -> unit) ->
  (Unix.file_descr * Unix.LargeFile.stats, Digestif.SHA1.t) Carton.Dec.t ->
  Bob_fpath.t ->
  Digestif.SHA1.t ->
  (Unix.file_descr * Unix.LargeFile.stats, Digestif.SHA1.t) Carton.Dec.t Fiber.t

val unpack :
  Bob_fpath.t ->
  status array ->
  ( string
    * int
    * Digestif.SHA1.t
    * (Unix.file_descr * Unix.LargeFile.stats, Digestif.SHA1.t) Carton.Dec.t,
    [> `No_root ] )
  result
  Fiber.t
*)
