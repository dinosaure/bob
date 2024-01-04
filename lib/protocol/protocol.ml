[@@@warning "-30"]

type 'a packet =
  | Ping : unit packet
  | Pong : unit packet
  | Quit : unit packet
  | Any : any packet

and any = Any : 'a packet * 'a -> any

let pp : type a. a packet Fmt.t =
 fun ppf -> function
  | Ping -> Fmt.string ppf "ping"
  | Pong -> Fmt.string ppf "pong"
  | Quit -> Fmt.string ppf "quit"
  | Any -> Fmt.string ppf "<any>"

let ping = Ping
let pong = Pong
let quit = Quit
let any = Any

type decoder = Decoder.decoder
type encoder = Encoder.encoder
type t = { decoder : decoder; encoder : encoder }
type error = [ Decoder.error | Encoder.error ]

let pp_error ppf = function
  | #Encoder.error as err -> Encoder.pp_error ppf err
  | #Decoder.error as err -> Decoder.pp_error ppf err

let state () = { decoder = Decoder.decoder (); encoder = Encoder.encoder () }
let ( <.> ) f g x = f (g x)

let decode_ping decoder =
  let ping decoder =
    match String.lowercase_ascii (Decoder.peek_packet decoder) with
    | "ping" ->
        Decoder.junk_packet decoder;
        Decoder.return () decoder
    | _ ->
        Decoder.junk_packet decoder;
        Decoder.fail `Invalid_packet decoder
  in
  Decoder.prompt ping decoder

let decode_pong decoder =
  let pong decoder =
    match String.lowercase_ascii (Decoder.peek_packet decoder) with
    | "pong" ->
        Decoder.junk_packet decoder;
        Decoder.return () decoder
    | _ ->
        Decoder.junk_packet decoder;
        Decoder.fail `Invalid_packet decoder
  in
  Decoder.prompt pong decoder

let decode_quit decoder =
  let quit decoder =
    match String.lowercase_ascii (Decoder.peek_packet decoder) with
    | "quit" ->
        Decoder.junk_packet decoder;
        Decoder.return () decoder
    | _ ->
        Decoder.junk_packet decoder;
        Decoder.fail `Invalid_packet decoder
  in
  Decoder.prompt quit decoder

let decode decoder =
  let any decoder =
    let packet = Decoder.peek_packet decoder in
    match
      List.map String.lowercase_ascii (Astring.String.cuts ~sep:" " packet)
    with
    | "ping" :: [] ->
        Decoder.junk_packet decoder;
        Decoder.return (Any (Ping, ()) : any) decoder
    | _ -> assert false
  in
  Decoder.prompt any decoder

let encode_ping encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"ping" encoder

let encode_pong encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"pong" encoder

let encode_quit encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"quit" encoder

let decode : type a. t -> a packet -> (a, [> Decoder.error ]) State.t =
 fun { decoder; _ } w ->
  let rec transl :
      (a, [> Decoder.error ]) Decoder.state -> (a, [> Decoder.error ]) State.t =
    function
    | Decoder.Done v -> State.Return v
    | Decoder.Read { buffer; off; len; continue } ->
        State.Read { buffer; off; len; continue = transl <.> continue }
    | Decoder.Error { error; _ } -> State.Error error
  in
  transl
    (match w with
    | Ping -> decode_ping decoder
    | Pong -> decode_pong decoder
    | Quit -> decode_quit decoder
    | Any -> decode decoder)

let rec encode :
    type a. t -> a packet -> a -> (unit, [> Encoder.error ]) State.t =
 fun ({ encoder; _ } as state) w v ->
  let rec transl :
      [> Encoder.error ] Encoder.state -> (unit, [> Encoder.error ]) State.t =
    function
    | Encoder.Write { buffer; off; len; continue } ->
        State.Write { buffer; off; len; continue = transl <.> continue }
    | Encoder.Done -> State.Return ()
    | Encoder.Error error -> State.Error error
  in
  match w with
  | Any ->
      let (Any (w, v)) = v in
      encode state w v
  | w ->
      transl
        (match w with
        | Ping -> encode_ping encoder v
        | Pong -> encode_pong encoder v
        | Quit -> encode_quit encoder v
        | Any ->
            assert
              false (* XXX(dinosaure): cannot occur. Polymorphic variant? *))
