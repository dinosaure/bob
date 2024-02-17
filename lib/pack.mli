type status
type decoder

type entry =
  int64
  * status
  * [ `Base of [ `A | `B | `C | `D ] * Carton.Dec.weight
    | `Ofs of int * Carton.Dec.weight * Carton.Dec.weight * Carton.Dec.weight
    | `Ref of
      Digestif.SHA1.t
      * Carton.Dec.weight
      * Carton.Dec.weight
      * Carton.Dec.weight ]

val ctx : decoder -> Digestif.SHA1.ctx
val is_base : status -> bool
val is_resolved : status -> bool
val offset_of_status : status -> int64
val kind_of_status : status -> [ `A | `B | `C | `D ]
val uid_of_status : status -> Digestif.SHA1.t
val pp_status : status Fmt.t

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
    instance) which deflates an entry (including its header). The given input
    (a {!Stream.source} or a {!Stream.stream}) must start at the beginning of
    the entry. It returns the deflated entry. An entry can be a [`Base] (and,
    in such case, you extract the entry) or a patch (and, in such case, you
    must reconstruct the entry with its source). *)

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

val pack :
  Bob_fpath.t ->
  status array ->
  (Unix.file_descr * Unix.LargeFile.stats, Digestif.SHA1.t) Carton.Dec.t Fiber.t
