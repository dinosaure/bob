[@@@warning "-30"]

type 'a packet = private
  | Ping : unit packet
  | Pong : unit packet
  | Hello : Capability.t list packet
  | Size : int64 packet
  | Kind : [ `Directory | `Files | `File ] packet
  | Ready : unit packet
  | Resume : (Digestif.SHA256.t * int64) packet
  | Quit : unit packet
  | Any : any packet

and any = Any : 'a packet * 'a -> any

type ('a, 'err) t =
  | Read of {
      buffer : bytes;
      off : int;
      len : int;
      continue : [ `End | `Len of int ] -> ('a, 'err) t;
    }
  | Write of {
      buffer : string;
      off : int;
      len : int;
      continue : int -> ('a, 'err) t;
    }
  | Return of 'a
  | Error of 'err

val ping : unit packet
val pong : unit packet
val hello : Capability.t list packet
val size : int64 packet
val kind : [ `Directory | `Files | `File ] packet
val ready : unit packet
val resume : (Digestif.SHA256.t * int64) packet
val quit : unit packet
val any : any packet
val pp : 'a packet Fmt.t

type decoder = Decoder.decoder
type encoder = Encoder.encoder
type ctx
type error = [ Decoder.error | Encoder.error ]

val pp_error : error Fmt.t
val send : ctx -> 'a packet -> 'a -> (unit, [> Encoder.error ]) t
val recv : ctx -> 'a packet -> ('a, [> Decoder.error ]) t
val return : 'a -> ('a, _) t
val fail : 'err -> (_, 'err) t
val state : unit -> ctx
val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
