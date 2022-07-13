type 'fd t

val make :
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  'fd ->
  'fd t

module type FLOW = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type flow
  type error
  type write_error = private [> `Closed ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
  val read : flow -> ([ `Data of Cstruct.t | `Eof ], error) result t
  val write : flow -> Cstruct.t -> (unit, write_error) result t
  val close : flow -> unit t
end

module Make (Flow : FLOW) : sig
  type error = [ `Rd of Flow.error | `Corrupted ]
  type write_error = [ `Closed | `Wr of Flow.write_error ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
  val recv : Flow.flow t -> ([ `Data of string | `End ], error) result Flow.t

  val send :
    Flow.flow t ->
    Stdbob.bigstring ->
    off:int ->
    len:int ->
    (int, write_error) result Flow.t

  val close : Flow.flow t -> unit Flow.t

  val getline :
    (char, Bigarray.int8_unsigned_elt) Ke.Rke.t ->
    Flow.flow t ->
    string option Flow.t
end
