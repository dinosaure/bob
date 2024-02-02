let src = Logs.Src.create "bob.protocol.decoder"

module Log = (val Logs.src_log src : Logs.LOG)

type decoder = { buffer : Bytes.t; mutable pos : int; mutable max : int }

let io_buffer_size = 65536
let decoder () = { buffer = Bytes.create io_buffer_size; pos = 0; max = 0 }

type ('v, 'err) state =
  | Done of 'v
  | Read of {
      buffer : bytes;
      off : int;
      len : int;
      continue : [ `End | `Len of int ] -> ('v, 'err) state;
    }
  | Error of 'err info

and 'err info = { error : 'err; buffer : bytes; committed : int }

type error =
  [ `End_of_input | `Not_enough_space | `Invalid_packet | `Partial_packet ]

let pp_error ppf = function
  | `End_of_input -> Fmt.string ppf "End of input"
  | `Not_enough_space -> Fmt.string ppf "Not enough space"
  | `Invalid_packet -> Fmt.string ppf "Invalid packet"
  | `Partial_packet -> Fmt.string ppf "Partial packet"

exception Leave of error info

let return (type v) (value : v) _ : (v, 'err) state = Done value

let rec map ~f = function
  | Error _ as err -> err
  | Done v -> Done (f v)
  | Read { buffer; off; len; continue } ->
      let continue v = map ~f (continue v) in
      Read { buffer; off; len; continue }

let ( let+ ) x f = map ~f x

let safe :
    (decoder -> ('v, ([> error ] as 'err)) state) -> decoder -> ('v, 'err) state
    =
 fun k decoder ->
  try k decoder
  with Leave { error = #error as error; buffer; committed } ->
    Error { error :> 'err; buffer; committed }

(* TODO(dinosaure): check [len] and avoid overflow. *)
let of_hex str ~off ~len =
  let of_chr = function
    | '0' .. '9' as chr -> Char.code chr - Char.code '0'
    | 'a' .. 'f' as chr -> Char.code chr - Char.code 'a' + 10
    | 'A' .. 'F' as chr -> Char.code chr - Char.code 'A' + 10
    | _ -> invalid_arg "Invalid hexadecimal character"
  in
  let value = ref 0 in
  for i = 0 to len - 1 do
    value := (!value lsl 4) + of_chr str.[off + i]
  done;
  !value

let at_least_one_packet decoder =
  if decoder.max - decoder.pos >= 4 then
    match
      of_hex (Bytes.unsafe_to_string decoder.buffer) ~off:decoder.pos ~len:4
    with
    | len when decoder.max - decoder.pos >= 4 + len -> Ok (Some len)
    | _ -> Ok None
    | exception _ ->
        Error
          {
            error = `Invalid_packet;
            buffer = decoder.buffer;
            committed = decoder.pos;
          }
  else Ok None

let fail error (decoder : decoder) : _ state =
  Error { error; buffer = decoder.buffer; committed = decoder.pos }

let leave error (decoder : decoder) =
  raise (Leave { error; buffer = decoder.buffer; committed = decoder.pos })

let prompt :
    (decoder -> ('v, ([> error ] as 'err)) state) -> decoder -> ('v, 'err) state
    =
 fun k decoder ->
  if decoder.pos > 0 then (
    let rest = decoder.max - decoder.pos in
    Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest;
    decoder.max <- rest;
    decoder.pos <- 0);
  let rec go off =
    if off = Bytes.length decoder.buffer then fail `Not_enough_space decoder
    else
      match at_least_one_packet { decoder with max = off } with
      | Ok None ->
          let continue = function
            | `Len len -> go (off + len)
            | `End -> fail `End_of_input decoder
          in
          Read
            {
              buffer = decoder.buffer;
              off;
              len = Bytes.length decoder.buffer - off;
              continue;
            }
      | Ok (Some _) ->
          decoder.max <- off;
          safe k decoder
      | Error err -> Error err
  in
  go decoder.max

let peek_packet decoder =
  match at_least_one_packet decoder with
  | Ok (Some len) ->
      Log.debug (fun m -> m "Got a packet of %d byte(s)." len);
      Bytes.sub_string decoder.buffer (decoder.pos + 4) len
  | Ok None -> leave `Partial_packet decoder
  | Error err -> raise (Leave err)

let junk_packet decoder =
  match at_least_one_packet decoder with
  | Ok (Some len) -> decoder.pos <- decoder.pos + 4 + len
  | Ok None -> leave `Partial_packet decoder
  | Error err -> raise (Leave err)
