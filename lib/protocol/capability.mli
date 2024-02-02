type t = [ `Version of int | `Resume | `Size ]

val to_string : t -> string
val of_string : string -> (t, [> `Msg of string ]) result
val common : t list -> t list -> t list
val has : t list -> t -> bool

type 'a key = Version : [ `Version of int ] key

val get : t list -> 'a key -> 'a option
