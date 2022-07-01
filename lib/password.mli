type t

val compile : string array -> t
val generate : ?g:Random.State.t -> t -> int -> string
