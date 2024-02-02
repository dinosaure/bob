type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
type t = [ value | `O of (string * t) list | `A of t list ]

val of_json : ?size_chunk:int -> in_channel -> (t, [> `Msg of string ]) result
val to_json : ?minify:bool -> ?size_chunk:int -> out_channel -> t -> unit
