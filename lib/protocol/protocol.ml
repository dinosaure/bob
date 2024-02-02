[@@@warning "-30"]

let src = Logs.Src.create "bob.protocol"

module Log = (val Logs.src_log src : Logs.LOG)
module SHA256 = Digestif.SHA256

type 'a packet =
  | Ping : unit packet
  | Pong : unit packet
  | Hello : Capability.t list packet
  | Size : int64 packet
  | Kind : [ `File | `Files | `Directory ] packet
  | Ready : unit packet
  | Resume : (SHA256.t * int64) packet
  | Quit : unit packet
  | Any : any packet

(* recv: Source
 * send: Stream *)
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

let return v = Return v
let fail err = Error err

let rec go ~f continue value =
  match continue value with
  | Return v -> f v
  | Read { buffer; off; len; continue } ->
      Read { buffer; off; len; continue = go ~f continue }
  | Write { buffer; off; len; continue } ->
      let k0 = function `End -> continue 0 | `Len len -> continue len in
      let k1 = function 0 -> go ~f k0 `End | len -> go ~f k0 (`Len len) in
      Write { buffer; off; len; continue = k1 }
  | Error err -> Error err

let bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t =
 fun m ~f ->
  match m with
  | Return v -> f v
  | Read { buffer; off; len; continue } ->
      Read { buffer; off; len; continue = go ~f continue }
  | Write { buffer; off; len; continue } ->
      let k0 = function `End -> continue 0 | `Len len -> continue len in
      let k1 = function 0 -> go ~f k0 `End | len -> go ~f k0 (`Len len) in
      Write { buffer; off; len; continue = k1 }
  | Error err -> Error err

let ( let* ) x f = bind x ~f

let pp : type a. a packet Fmt.t =
 fun ppf -> function
  | Ping -> Fmt.string ppf "ping"
  | Pong -> Fmt.string ppf "pong"
  | Hello -> Fmt.string ppf "hello"
  | Size -> Fmt.string ppf "size"
  | Kind -> Fmt.string ppf "kind"
  | Ready -> Fmt.string ppf "ready"
  | Resume -> Fmt.string ppf "resume"
  | Quit -> Fmt.string ppf "quit"
  | Any -> Fmt.string ppf "<any>"

let ping = Ping
let pong = Pong
let hello = Hello
let size = Size
let kind = Kind
let ready = Ready
let resume = Resume
let quit = Quit
let any = Any

type decoder = Decoder.decoder
type encoder = Encoder.encoder
type ctx = { decoder : decoder; encoder : encoder }
type error = [ Decoder.error | Encoder.error ]

let pp_error ppf = function
  | #Encoder.error as err -> Encoder.pp_error ppf err
  | #Decoder.error as err -> Decoder.pp_error ppf err

let state () = { decoder = Decoder.decoder (); encoder = Encoder.encoder () }
let ( <.> ) f g x = f (g x)

let decode_ping decoder =
  match String.lowercase_ascii (Decoder.peek_packet decoder) with
  | "ping" ->
      Decoder.junk_packet decoder;
      Decoder.return () decoder
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_pong decoder =
  match String.lowercase_ascii (Decoder.peek_packet decoder) with
  | "pong" ->
      Decoder.junk_packet decoder;
      Decoder.return () decoder
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_quit decoder =
  match String.lowercase_ascii (Decoder.peek_packet decoder) with
  | "quit" ->
      Decoder.junk_packet decoder;
      Decoder.return () decoder
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_hello decoder =
  let packet = String.lowercase_ascii (Decoder.peek_packet decoder) in
  match Astring.String.cuts ~sep:" " ~empty:false packet with
  | "hello" :: capabilities ->
      let capabilities =
        List.fold_left
          (fun acc str ->
            match Capability.of_string str with
            | Ok capability -> capability :: acc
            | Error (`Msg err) ->
                Log.warn (fun m -> m "%s" err);
                acc)
          [] capabilities
      in
      Decoder.junk_packet decoder;
      Decoder.return capabilities decoder
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_size decoder =
  let packet = String.lowercase_ascii (Decoder.peek_packet decoder) in
  match Astring.String.cuts ~sep:" " ~empty:false packet with
  | [ "size"; n ] -> (
      match Int64.of_string_opt n with
      | Some n ->
          Decoder.junk_packet decoder;
          Decoder.return n decoder
      | None -> Decoder.fail `Invalid_packet decoder)
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_kind decoder =
  let packet = String.lowercase_ascii (Decoder.peek_packet decoder) in
  match Astring.String.cuts ~sep:" " ~empty:false packet with
  | [ "kind"; "file" ] ->
      Decoder.junk_packet decoder;
      Decoder.return `File decoder
  | [ "kind"; "files" ] ->
      Decoder.junk_packet decoder;
      Decoder.return `Files decoder
  | [ "kind"; "directory" ] ->
      Decoder.junk_packet decoder;
      Decoder.return `Directory decoder
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_ready decoder =
  let packet = String.lowercase_ascii (Decoder.peek_packet decoder) in
  match packet with
  | "ready" ->
      Decoder.junk_packet decoder;
      Decoder.return () decoder
  | _ -> Decoder.fail `Invalid_packet decoder

let decode_resume decoder =
  let packet = String.lowercase_ascii (Decoder.peek_packet decoder) in
  match Astring.String.cuts ~sep:" " packet with
  | [ "resume"; hash; cursor ] -> (
      try
        let hash = SHA256.of_hex hash in
        let cursor = Int64.of_string cursor in
        Decoder.junk_packet decoder;
        Decoder.return (hash, cursor) decoder
      with _ -> Decoder.fail `Invalid_packet decoder)
  | _ -> Decoder.fail `Invalid_packet decoder

let decode decoder =
  let any dec : (any, _) Decoder.state =
    let packet = Decoder.peek_packet dec in
    let open Decoder in
    match
      List.map String.lowercase_ascii (Astring.String.cuts ~sep:" " packet)
    with
    | "ping" :: _ ->
        let+ v = decode_ping dec in
        (Any (Ping, v) : any)
    | "pong" :: _ ->
        let+ v = decode_pong dec in
        (Any (Pong, v) : any)
    | "hello" :: _ ->
        let+ v = decode_hello dec in
        (Any (Hello, v) : any)
    | "size" :: _ ->
        let+ v = decode_size dec in
        (Any (Size, v) : any)
    | "kind" :: _ ->
        let+ v = decode_kind dec in
        (Any (Kind, v) : any)
    | "ready" :: _ ->
        let+ v = decode_ready dec in
        (Any (Ready, v) : any)
    | "resume" :: _ ->
        let+ v = decode_resume dec in
        (Any (Resume, v) : any)
    | "quit" :: _ ->
        let+ v = decode_quit dec in
        (Any (Quit, v) : any)
    | str ->
        let str = String.concat " " str in
        Log.err (fun m -> m "Got an invalid packet: %S" str);
        Decoder.fail `Invalid_packet dec
  in
  Decoder.prompt any decoder

let encode_ping encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"ping" encoder

let encode_pong encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"pong" encoder

let encode_ready encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"ready" encoder

let encode_quit encoder () =
  Encoder.write (Fun.const Encoder.Done) ~pkt:"quit" encoder

let encode_hello encoder capabilities =
  let capabilities = List.map Capability.to_string capabilities in
  let pkt = String.concat " " ("hello" :: capabilities) in
  Encoder.write (Fun.const Encoder.Done) ~pkt encoder

let encode_size encoder n =
  let pkt = Fmt.str "size %Ld" n in
  Encoder.write (Fun.const Encoder.Done) ~pkt encoder

let encode_kind encoder k =
  let pkt =
    match k with
    | `File -> "kind file"
    | `Files -> "kind files"
    | `Directory -> "kind directory"
  in
  Encoder.write (Fun.const Encoder.Done) ~pkt encoder

let encode_resume encoder (hash, cursor) =
  let pkt = Fmt.str "resume %a %Ld" SHA256.pp hash cursor in
  Encoder.write (Fun.const Encoder.Done) ~pkt encoder

let recv : type a. ctx -> a packet -> (a, [> Decoder.error ]) t =
 fun { decoder; _ } w ->
  let rec transl :
      (a, [> Decoder.error ]) Decoder.state -> (a, [> Decoder.error ]) t =
    function
    | Decoder.Done v -> Return v
    | Decoder.Read { buffer; off; len; continue } ->
        Read { buffer; off; len; continue = transl <.> continue }
    | Decoder.Error { error; _ } -> Error error
  in
  transl
    (match w with
    | Ping -> Decoder.prompt decode_ping decoder
    | Pong -> Decoder.prompt decode_pong decoder
    | Quit -> Decoder.prompt decode_quit decoder
    | Hello -> Decoder.prompt decode_hello decoder
    | Size -> Decoder.prompt decode_size decoder
    | Kind -> Decoder.prompt decode_kind decoder
    | Ready -> Decoder.prompt decode_ready decoder
    | Resume -> Decoder.prompt decode_resume decoder
    | Any -> decode decoder)

let rec send : type a. ctx -> a packet -> a -> (unit, [> Encoder.error ]) t =
 fun ({ encoder; _ } as state) w v ->
  let rec transl :
      [> Encoder.error ] Encoder.state -> (unit, [> Encoder.error ]) t =
    function
    | Encoder.Write { buffer; off; len; continue } ->
        Write { buffer; off; len; continue = transl <.> continue }
    | Encoder.Done -> Return ()
    | Encoder.Error error -> Error error
  in
  match w with
  | Any ->
      let (Any (w, v)) = v in
      send state w v
  | w ->
      transl
        (match w with
        | Ping -> encode_ping encoder v
        | Pong -> encode_pong encoder v
        | Quit -> encode_quit encoder v
        | Hello -> encode_hello encoder v
        | Size -> encode_size encoder v
        | Kind -> encode_kind encoder v
        | Ready -> encode_ready encoder v
        | Resume -> encode_resume encoder v
        | Any ->
            assert
              false (* XXX(dinosaure): cannot occur. Polymorphic variant? *))
