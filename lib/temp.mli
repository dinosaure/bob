val set_default_directory : Fpath.t -> unit
val get_default_directory : unit -> Fpath.t

type pattern = (string -> string, Format.formatter, unit, string) format4

val random_temporary_path : ?g:Random.State.t -> pattern -> Fpath.t
