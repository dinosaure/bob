type t

val compare : t -> t -> int
val pp : t Fmt.t
val current : t -> Digestif.SHA256.t * int64

val v1 :
  cur:Digestif.SHA256.t * int64 -> res:Digestif.SHA256.t * int64 -> Fpath.t -> t

val of_json : in_channel -> (t, [> `Msg of string ]) result
val to_json : out_channel -> t -> unit
