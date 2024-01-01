type 'a send =
  | Ping : unit send
  | Pong : unit send

type 'a recv =
  | Ping : unit recv
  | Pong : unit recv

type decoder = Decoder.decoder
type encoder = Encoder.encoder

let ( <.> ) f g = fun x -> f (g x)

let decode_ping _ = assert false
let decode_pong _ = assert false

let decode : type a. decoder -> a recv -> (a, [> Decoder.error ]) State.t =
  fun decoder w ->
  let rec transl :
    (a, [> Decoder.error ]) Decoder.state ->
    (a, [> Decoder.error ]) State.t = function
    | Decoder.Done v -> State.Return v
    | Decoder.Read { buffer; off; len; continue } ->
      State.Read { buffer; off; len; continue= transl <.> continue }
    | Decoder.Error { error;_ } -> State.Error error in
  transl begin match w with
    | Ping -> decode_ping decoder
    | Pong -> decode_pong decoder end

let encode : type a. encoder -> a send -> a -> (a, [> Encoder.error ]) State.t =
  fun encoder w v ->
  let rec transl :
    (a, [> Encoder.error ]) Encoder.state ->
    (a, [> Encoder.error ]) State.t = function
    | Encoder.Write { buffer; off; len; continue } ->
      State.Write { buffer; off; len; continue= transl <.> continue }
    | Encoder.Done -> State.Return ()
    | Encoder.Error error -> State.Error error in
  transl begin match w with
    | Ping -> encode_ping encoder v
    | Pong -> encode_pong encoder v end
