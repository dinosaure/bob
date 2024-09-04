type t

val v1 : ?size:int64 -> [ `Directory | `File | `Files ] -> t
val size_of_document : t -> int64 option
val kind_of_document : t -> [ `Directory | `File | `Files ]
val pp : t Fmt.t
val equal : t -> t -> bool
