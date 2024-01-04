[@@@warning "-30"]

type 'a packet = private
  | Ping : unit packet
  | Pong : unit packet
  | Quit : unit packet
  | Any : any packet

and any = Any : 'a packet * 'a -> any

val ping : unit packet
val pong : unit packet
val quit : unit packet
val any : any packet
val pp : 'a packet Fmt.t

type decoder = Decoder.decoder
type encoder = Encoder.encoder
type t
type error = [ Decoder.error | Encoder.error ]

val pp_error : error Fmt.t
val encode : t -> 'a packet -> 'a -> (unit, [> Encoder.error ]) State.t
val decode : t -> 'a packet -> ('a, [> Decoder.error ]) State.t
val state : unit -> t
