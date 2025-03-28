type elt =
  [ `Reg of Bob_fpath.t * Carton.Uid.t | `Dir of Bob_fpath.t * Carton.Uid.t ]

type tree = elt list

val tree_of_string : ?path:Bob_fpath.t -> string -> tree
val tree_of_bstr : ?path:Bob_fpath.t -> Stdbob.bigstring -> elt Stream.source
val digest : Carton.First_pass.digest
val identify : Digestif.SHA1.ctx Carton.First_pass.identify
val serialize_directory : (Bob_fpath.t * Digestif.SHA1.t) list -> string list

val hash_of_root :
  real_length:int -> root:Bob_fpath.t -> Digestif.SHA1.t -> Digestif.SHA1.t

val hash_of_directory :
  root:Bob_fpath.t ->
  (Bob_fpath.t, Digestif.SHA1.t * [ `Dir | `Reg | `Root ]) Hashtbl.t ->
  Bob_fpath.t ->
  Digestif.SHA1.t Fiber.t

val hash_of_filename : Bob_fpath.t -> Digestif.SHA1.t Fiber.t
