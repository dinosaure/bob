type error =
  [ Bob_unix.error
  | `Connect of Unix.error
  | `Crypto of
    [ `Closed
    | `Corrupted
    | `Rd of Unix.error
    | `Wr of [ `Closed | `Unix of Unix.error ] ] ]

val pp_error : error Fmt.t
val open_error : ('a, error) result -> ('a, [> error ]) result
val sockaddr_with_secure_port : Unix.sockaddr -> int -> Unix.sockaddr

val transfer :
  ?chunk:int ->
  ?reporter:(int -> unit Fiber.t) ->
  identity:string ->
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  Unix.sockaddr ->
  Stdbob.bigstring Stream.stream ->
  (unit, error) result Fiber.t

val receive :
  ?reporter:(int -> unit Fiber.t) ->
  ?finalise:(unit -> unit) ->
  identity:string ->
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  Unix.sockaddr ->
  (Stdbob.bigstring Stream.source, error) result Fiber.t
