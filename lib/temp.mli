val set_default_directory : Fpath.t -> unit
(** [set_default_directory path] sets the global {i temp} directory. *)

val get_default_directory : unit -> Fpath.t
(** [get_default_directory ()] returns the global {i temp} directory. *)

type pattern = (string -> string, Format.formatter, unit, string) format4

val random_temporary_path : ?g:Random.State.t -> pattern -> Fpath.t
(** [random_temporary_path ?g pattern] generates a new path which does not exists into
    the global {i temp} directory. *)
