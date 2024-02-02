type elt =
  [ `Reg of Bob_fpath.t * Digestif.SHA1.t
  | `Dir of Bob_fpath.t * Digestif.SHA1.t ]

type tree = elt list

val tree_of_string : ?path:Bob_fpath.t -> string -> tree
val tree_of_cstruct : ?path:Bob_fpath.t -> Stdbob.bigstring -> elt Stream.source

val digest :
  kind:Carton.kind ->
  ?off:int ->
  ?len:int ->
  Stdbob.bigstring ->
  Digestif.SHA1.t

val serialize_directory :
  (Bob_fpath.t * Digestif.SHA1.t) list -> string Stream.stream

val hash_of_root :
  real_length:int -> root:Bob_fpath.t -> Digestif.SHA1.t -> Digestif.SHA1.t

val hash_of_directory :
  root:Bob_fpath.t ->
  (Bob_fpath.t, Digestif.SHA1.t * [ `Dir | `Reg | `Root ]) Hashtbl.t ->
  Bob_fpath.t ->
  Digestif.SHA1.t Fiber.t

val hash_of_filename : Bob_fpath.t -> Digestif.SHA1.t Fiber.t
