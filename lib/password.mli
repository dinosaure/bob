(** {1: Human-readable password generator.}

    This module implements a password generator which is human readable: it
    mixes {b syllable} from a dictionary {!type:t}. *)

type t
(** Type of dictionaries. *)

val compile : string array -> t
(** [compile entries] compiles a suit of words into a {{!type:t} dictionary}. *)

val generate : ?g:Random.State.t -> t -> int -> string
(** [generate ?g t len] generates a {b weak} password with [len] {i syllable}
    from a {{!type:t} dictionary} and a possible {!Random.State.t} (otherwise,
    we generate one with {!val:Random.State.make_self_init}). *)
