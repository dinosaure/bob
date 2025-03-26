open Stdbob

type t

val create : int -> t
val size : t -> int
val is_empty : t -> bool
val compress : t -> unit
val peek : t -> bigstring list
val push : t -> ?off:int -> ?len:int -> bigstring -> unit
val push_string : t -> string -> unit
val keep : t -> ?off:int -> ?len:int -> bigstring -> unit
val keep_bytes : t -> ?off:int -> ?len:int -> bytes -> unit
val shift : t -> int -> unit
