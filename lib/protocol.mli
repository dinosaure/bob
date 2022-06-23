type ctx

val make : unit -> ctx
val pp : ctx Fmt.t

val income_is_empty : ctx -> bool

type error =
  [ `Not_enough_space
  | `End_of_input
  | `Invalid_packet of int
  | `Invalid_header 
  | `Invalid_new_server
  | `Invalid_uid
  | Spoke.error ]

val pp_error : error Fmt.t

type 'a t =
  | Rd of { buf : bytes; off : int; len : int; k : 'a krd }
  | Wr of { str : string; off : int; len : int; k : 'a kwr }
  | Done of 'a 
  | Fail of error
and 'a krd = [ `End | `Len of int ] -> 'a t
and 'a kwr = int -> 'a t

val send_string : ctx -> string -> unit t
val send_packet : ctx -> int * State.raw -> unit t
val recv : ctx -> (int * State.raw) t
val save : ctx -> [ `End | `Data of string * int * int ] -> unit
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val return : 'a -> 'a t
