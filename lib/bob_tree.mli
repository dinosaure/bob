type t
type entry = [ `Dir of string | `File of string ]

type elt =
  | Filename of string
  | Folder of { dirname : string; entries : entry list }
  | Tree of { abs : Bob_fpath.t; entries : entry list }
  | Blob of Bob_fpath.t

val setup_paths :
  string list -> [ `Error of bool * string | `Ok of Bob_fpath.t list ]

val make : cwd:Bob_fpath.t -> Bob_fpath.t list -> t
val flatten : t -> elt list
