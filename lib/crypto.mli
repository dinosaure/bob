type t

val make : ciphers:(Spoke.cipher * Spoke.cipher) -> shared_keys:(string * string) -> Unix.file_descr -> t
val recv : t -> ([ `Data of string | `End ], [ `Rd of Unix.error | `Corrupted ]) result Fiber.t
val send : t -> string -> off:int -> len:int -> (int, [ `Closed | `Unix of Unix.error ]) result Fiber.t
val close : t -> unit Fiber.t
