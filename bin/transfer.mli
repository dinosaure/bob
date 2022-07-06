type error =
  [ Bob_unix.error
  | `Connect of Connect.error
  | `Crypto of
    [ `Closed
    | `Corrupted
    | `Rd of Unix.error
    | `Wr of [ `Closed | `Unix of Unix.error ] ] ]

val pp_error : error Fmt.t
val sockaddr_with_secure_port : Unix.sockaddr -> int -> Unix.sockaddr

val transfer :
  ?chunk:int ->
  ?reporter:(int -> unit) ->
  identity:string ->
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  Unix.sockaddr ->
  Fpath.t ->
  (unit, error) result Fiber.t

val save :
  ?g:Random.State.t ->
  ?tmp:Pack.pattern ->
  ?reporter:(int -> unit) ->
  identity:string ->
  ciphers:Spoke.cipher * Spoke.cipher ->
  shared_keys:string * string ->
  Unix.sockaddr ->
  (Fpath.t, error) result Fiber.t
