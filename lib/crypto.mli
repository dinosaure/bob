type t
type error = [ `Rd of Unix.error | `Corrupted ]
type write_error = [ `Closed | `Unix of Unix.error ]

val pp_error : error Fmt.t
val pp_write_error : write_error Fmt.t

val make :
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  Unix.file_descr ->
  t

val recv : t -> ([ `Data of string | `End ], error) result Fiber.t

val send :
  t -> string -> off:int -> len:int -> (int, write_error) result Fiber.t

val close : t -> unit Fiber.t

val getline :
  (char, Bigarray.int8_unsigned_elt) Ke.Rke.t -> t -> string option Fiber.t
