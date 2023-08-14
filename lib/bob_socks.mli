type addr =
  [ `Inet of Unix.inet_addr * int | `Domain of [ `host ] Domain_name.t * int ]

type server

val parser : string -> (server, [> `Msg of string ]) result
val pp : server Fmt.t

val connect :
  happy_eyeballs:Bob_happy_eyeballs.t ->
  server:server ->
  addr ->
  ( Unix.sockaddr * Unix.file_descr,
    [> `Closed | `Msg of string | `Unix of Unix.error ] )
  result
  Fiber.t
