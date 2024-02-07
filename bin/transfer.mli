type error =
  [ Bob_unix.error
  | `Blocking_connect of Connect.error
  | `Connect of [ `Closed | `Msg of string | `Unix of Unix.error ]
  | `Crypto of
    [ `Closed
    | `Corrupted
    | `Rd of Unix.error
    | `Wr of [ `Closed | `Unix of Unix.error ] ] ]

val pp_error : error Fmt.t
val open_error : ('a, error) result -> ('a, [> error ]) result
val addr_with_secure_port : Bob_socks.addr -> int -> Bob_socks.addr
val max_packet : int

val transfer :
  ?chunk:int ->
  ?reporter:(int -> unit Fiber.t) ->
  identity:string ->
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  happy_eyeballs:Bob_happy_eyeballs.t ->
  ?through:Bob_socks.server ->
  Bob_socks.addr ->
  Stdbob.bigstring Stream.stream ->
  (unit, error) result Fiber.t

val receive :
  ?reporter:(int -> unit Fiber.t) ->
  ?finalise:(unit -> unit) ->
  identity:string ->
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  happy_eyeballs:Bob_happy_eyeballs.t ->
  ?through:Bob_socks.server ->
  Bob_socks.addr ->
  (Stdbob.bigstring Stream.source, error) result Fiber.t
