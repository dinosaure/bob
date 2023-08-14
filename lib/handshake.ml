let src = Logs.Src.create "bob.handshake"

module Log = (val Logs.src_log src : Logs.LOG)

type ctx = {
  mutable ic_buffer : bytes;
  mutable ic_pos : int;
  mutable ic_max : int;
  oc_buffer : bytes;
  mutable oc_pos : int;
}

let pp ppf ctx =
  Fmt.pf ppf
    "{ @[<hov>ic_buffer=@[<hov>%a@];@ ic_pos=%d;@ ic_max=%d;@ \
     oc_buffer=@[<hov>%a@];@ oc_pos=%d;@] }"
    (Hxd_string.pp Hxd.default)
    (Bytes.to_string ctx.ic_buffer)
    ctx.ic_pos ctx.ic_max
    (Hxd_string.pp Hxd.default)
    (Bytes.to_string ctx.oc_buffer)
    ctx.oc_pos

let income_is_empty { ic_pos; ic_max; _ } = ic_max - ic_pos = 0

(* XXX(dinosaure): the longest packet should be the [New_server _].
   [04x][02x][04x][public:34 bytes][identity:<=47 bytes][\000] <= 92 bytes.
   Nevermind, it's [Y_and_server_validator]:
   [04x][02x][Y:32 bytes][validator:64 bytes] <= 102 *)

let save ctx = function
  | `End | `Data (_, _, 0) -> ()
  | `Data (str, off, len) ->
      if ctx.ic_pos > 0 then (
        let rest = ctx.ic_max - ctx.ic_pos in
        Bytes.blit ctx.ic_buffer ctx.ic_pos ctx.ic_buffer 0 rest;
        Bytes.fill ctx.ic_buffer rest (Bytes.length ctx.ic_buffer - rest) '\000';
        ctx.ic_max <- rest;
        ctx.ic_pos <- 0);
      let free = Bytes.length ctx.ic_buffer - ctx.ic_max in
      if len <= free then (
        Bytes.blit_string str off ctx.ic_buffer ctx.ic_max len;
        ctx.ic_max <- ctx.ic_max + len)
      else
        let plus = len - free in
        let ic_buffer = Bytes.create (Bytes.length ctx.ic_buffer + plus) in
        Bytes.blit ctx.ic_buffer 0 ic_buffer 0 ctx.ic_max;
        Bytes.blit_string str off ic_buffer ctx.ic_max len;
        ctx.ic_buffer <- ic_buffer;
        ctx.ic_max <- ctx.ic_max + len
(* XXX(dinosaure): this is the worst case where something pending into the
   [ic_buffer] & the flow but we must save it for whatever reason. We probably
   want to break the control flow to:
   1) ask the client what he/she wants to do from an Agreement
   2) do the same for the relay where a secure room allocation is required

   A possible attack exists so which leads to an [Out_of_memory] if the user
   spams the relay with multiples packets and an agreement is found. However,
   according to our implementation [ic_buffer] should not be bigger than
   65535 byte(s) (the length of the buffer's flow) and OCaml is able to
   handle it _nicely_ - however, if we reach an [Out_of_memory] from this
   place, what I just said is wrong... *)

let make () =
  {
    ic_buffer = Bytes.create (102 * 4);
    ic_pos = 0;
    ic_max = 0;
    oc_buffer = Bytes.create (102 * 4);
    oc_pos = 0;
  }

type error =
  [ `Not_enough_space
  | `End_of_input
  | `Invalid_packet of int
  | `Invalid_header
  | `Invalid_new_server
  | `Invalid_uid
  | `Unexpected_packet
  | Spoke.error ]

let pp_error ppf = function
  | `Not_enough_space -> Fmt.string ppf "Not enough space"
  | `End_of_input -> Fmt.string ppf "End of input"
  | `Invalid_packet packet -> Fmt.pf ppf "Invalid packet (%02x)" packet
  | `Invalid_header -> Fmt.pf ppf "Invalid header"
  | `Invalid_new_server -> Fmt.string ppf "Invalid new server"
  | `Invalid_uid -> Fmt.string ppf "Invalid UID"
  | `Unexpected_packet -> Fmt.string ppf "Unexpected packet"
  | #Spoke.error as err -> Spoke.pp_error ppf err

type 'a t =
  | Rd of { buf : bytes; off : int; len : int; k : 'a krd }
  | Wr of { str : string; off : int; len : int; k : 'a kwr }
  | Done of 'a
  | Fail of error

and 'a krd = [ `End | `Len of int ] -> 'a t
and 'a kwr = int -> 'a t

let ( >>= ) =
  let rec go f m len =
    match m len with
    | Done v -> f v
    | Fail err -> Fail err
    | Rd { buf; off; len; k } -> Rd { buf; off; len; k = go f k }
    | Wr { str; off; len; k } ->
        let k0 = function `End -> k 0 | `Len len -> k len in
        let k1 = function 0 -> go f k0 `End | len -> go f k0 (`Len len) in
        Wr { str; off; len; k = k1 }
  in
  fun m f ->
    match m with
    | Done v -> f v
    | Fail err -> Fail err
    | Rd { buf; off; len; k } -> Rd { buf; off; len; k = go f k }
    | Wr { str; off; len; k } ->
        let k0 = function `End -> k 0 | `Len len -> k len in
        let k1 = function 0 -> go f k0 `End | len -> go f k0 (`Len len) in
        Wr { str; off; len; k = k1 }

let return v = Done v

exception Leave of error

let leave_with _ctx error = raise (Leave error)
let safe k ctx = try k ctx with Leave err -> Fail err

let flush k0 ctx =
  if ctx.oc_pos > 0 then
    let rec k1 n =
      if n < ctx.oc_pos then
        Wr
          {
            str = Bytes.unsafe_to_string ctx.oc_buffer;
            off = n;
            len = ctx.oc_pos - n;
            k = (fun m -> k1 (n + m));
          }
      else (
        ctx.oc_pos <- 0;
        k0 ctx)
    in
    k1 0
  else k0 ctx

let write_string ctx str =
  let max = Bytes.length ctx.oc_buffer in
  if String.length str > max - ctx.oc_pos then leave_with ctx `Not_enough_space;
  Bytes.blit_string str 0 ctx.oc_buffer ctx.oc_pos (String.length str);
  ctx.oc_pos <- ctx.oc_pos + String.length str

let write_fmt ctx fmt = Fmt.kstr (write_string ctx) fmt
let done_unit = Done ()

let send_string ctx str =
  safe
    (fun ctx ->
      write_string ctx str;
      flush (Stdbob.always done_unit) ctx)
    ctx

let uid_of_packet = function
  | `Hello_as_a_server _ -> 00
  | `Hello_as_a_client -> 01
  | `Server_identity _ -> 02
  | `Client_identity _ -> 03
  | `New_server _ -> 04
  | `Client_validator _ -> 05
  | `Y_and_server_validator _ -> 06
  | `X_and_client_identity _ -> 07
  | `Agreement -> 08
  | `Closed _ -> 09
  | `Accepted -> 10
  | `Refused -> 11
  | `Relay_failure _ -> 12
  | `Spoke_failure _ -> 13
  | `Done -> 14
  | `Timeout -> 15

let pp_packet ppf = function
  | `Hello_as_a_server public -> Fmt.string ppf (Spoke.public_to_string public)
  | `Hello_as_a_client -> ()
  | `Server_identity str -> Fmt.pf ppf "%s\000" str
  | `Client_identity str -> Fmt.pf ppf "%s\000" str
  | `New_server (uid, public, server_identity) ->
      Fmt.pf ppf "%04x%s%s\000" uid
        (Spoke.public_to_string public)
        server_identity
  | `Client_validator str -> Fmt.string ppf str
  | `Y_and_server_validator (_Y, server_validator) ->
      assert (String.length _Y = 32);
      assert (String.length server_validator = 64);
      Fmt.pf ppf "%s%s" _Y server_validator
  | `X_and_client_identity (_X, client_identity) ->
      Fmt.pf ppf "%s%s\000" _X client_identity
  | `Agreement -> ()
  | `Closed uid -> Fmt.pf ppf "%04x" uid
  | `Accepted -> ()
  | `Refused -> ()
  | `Done -> ()
  | `Timeout -> ()
  | `Relay_failure (`Invalid_client uid) -> Fmt.pf ppf "C%04x" uid
  | `Relay_failure (`Invalid_server uid) -> Fmt.pf ppf "S%04x" uid
  | `Relay_failure (`No_handshake_with uid) -> Fmt.pf ppf "H%04x" uid
  | `Relay_failure `No_agreement -> Fmt.pf ppf "N0000"
  | `Spoke_failure `Point_is_not_on_prime_order_subgroup ->
      Fmt.string ppf "\000"
  | `Spoke_failure `Invalid_client_validator -> Fmt.string ppf "\001"
  | `Spoke_failure `Invalid_server_validator -> Fmt.string ppf "\002"
  | `Spoke_failure `Invalid_public_packet -> Fmt.string ppf "\003"
  | `Spoke_failure `Invalid_secret_packet -> Fmt.string ppf "\004"

let send_packet ctx (uid, packet) =
  safe
    (fun ctx ->
      write_fmt ctx "%04x" uid;
      write_fmt ctx "%02x" (uid_of_packet packet);
      write_fmt ctx "%a" pp_packet packet;
      flush (Stdbob.always done_unit) ctx)
    ctx

let len_of_packet = function
  | 00 -> `Fixed 34
  | 01 -> `Fixed 0
  | 02 -> `Until (0, '\000')
  | 03 -> `Until (0, '\000')
  | 04 -> `Until (4 + 34, '\000')
  | 05 -> `Fixed 64
  | 06 -> `Fixed 96
  | 07 -> `Until (32, '\000')
  | 08 -> `Fixed 0
  | 09 -> `Fixed 4
  | 10 -> `Fixed 0
  | 11 -> `Fixed 0
  | 12 -> `Fixed 5
  | 13 -> `Fixed 1
  | 14 -> `Fixed 0
  | 15 -> `Fixed 0
  | _ -> invalid_arg "Invalid packet"

let hex_of_chr = function
  | '0' .. '9' as chr -> Char.code chr - Char.code '0'
  | 'a' .. 'f' as chr -> 10 + Char.code chr - Char.code 'a'
  | 'A' .. 'F' as chr -> 10 + Char.code chr - Char.code 'A'
  | _ -> invalid_arg "Invalid hex"

let of_hex ~off ~len buf =
  if len mod 2 <> 0 then Error (`Msg "hex sequence must be of even length")
  else
    try
      let res = ref 0 in
      for i = 0 to (len / 2) - 1 do
        let c0 = hex_of_chr (Bytes.get buf (off + (2 * i))) in
        let c1 = hex_of_chr (Bytes.get buf (off + (2 * i) + 1)) in
        res := (!res * 256) + ((c0 lsl 4) lor c1)
      done;
      Ok !res
    with Invalid_argument _ ->
      Error (`Msg (Fmt.str "Invalid hex: %S" (Bytes.sub_string buf off len)))

let of_hex_exn ~off ~len buf =
  match of_hex ~off ~len buf with Ok v -> v | Error (`Msg err) -> failwith err

let exists ~chr ~off ctx =
  let pos = ref (off + ctx.ic_pos) in
  let nil =
    match chr with
    | '\xff' -> '\x00'
    | chr -> Char.unsafe_chr (Char.code chr + 1)
  in
  let tmp = ref nil in
  while
    !pos < ctx.ic_max
    &&
    (tmp := Bytes.get ctx.ic_buffer !pos;
     !tmp <> chr)
  do
    incr pos
  done;
  !pos < ctx.ic_max && !tmp = chr

let prompt_fixed ~len:required k ctx =
  let rec go off =
    if off > Bytes.length ctx.ic_buffer then Fail `Not_enough_space
    else if off - ctx.ic_pos < 6 + required then
      let k = function
        | `Len len -> go (off + len)
        | `End -> Fail `End_of_input
      in
      Rd { buf = ctx.ic_buffer; off; len = Bytes.length ctx.ic_buffer - off; k }
    else (
      ctx.ic_max <- off;
      safe k ctx)
  in
  go ctx.ic_max

let prompt_until ~min:required ~chr k ctx =
  let rec go off =
    if off > Bytes.length ctx.ic_buffer then Fail `Not_enough_space
    else if
      off - ctx.ic_pos < 6 + required
      || not (exists ~chr ~off:required { ctx with ic_max = off })
    then
      let k = function
        | `Len len -> go (off + len)
        | `End -> Fail `End_of_input
      in
      Rd { buf = ctx.ic_buffer; off; len = Bytes.length ctx.ic_buffer - off; k }
    else (
      ctx.ic_max <- off;
      safe k ctx)
  in
  go ctx.ic_max

let prompt k ctx =
  if ctx.ic_pos > 0 then (
    let rest = ctx.ic_max - ctx.ic_pos in
    Bytes.blit ctx.ic_buffer ctx.ic_pos ctx.ic_buffer 0 rest;
    Bytes.fill ctx.ic_buffer rest (Bytes.length ctx.ic_buffer - rest) '\000';
    ctx.ic_max <- rest;
    ctx.ic_pos <- 0);
  let rec go off =
    if off > Bytes.length ctx.ic_buffer then Fail `Not_enough_space
    else if off - ctx.ic_pos < 6 (* [%04x%02x] *) then
      let k = function
        | `Len len -> go (off + len)
        | `End -> Fail `End_of_input
      in
      Rd { buf = ctx.ic_buffer; off; len = Bytes.length ctx.ic_buffer - off; k }
    else
      match
        ( of_hex ~off:ctx.ic_pos ~len:4 ctx.ic_buffer,
          of_hex ~off:(ctx.ic_pos + 4) ~len:2 ctx.ic_buffer )
      with
      | Ok _, Ok packet -> (
          match len_of_packet packet with
          | `Fixed 000 ->
              ctx.ic_max <- off;
              safe k ctx
          | `Fixed len ->
              ctx.ic_max <- off;
              prompt_fixed ~len k ctx
          | `Until (min, chr) ->
              ctx.ic_max <- off;
              prompt_until ~min:(min + 6) ~chr k ctx
          | exception _ -> Fail (`Invalid_packet packet))
      | Error _, _ | _, Error _ -> Fail `Invalid_header
  in
  go ctx.ic_max

let packet_of_bytes ~pkt ~off buf =
  match pkt with
  | 00 -> (
      let public = Bytes.sub_string buf off 34 in
      match Spoke.public_of_string public with
      | Ok public -> Ok (34, `Hello_as_a_server public)
      | Error err -> Error err)
  | 01 -> Ok (0, `Hello_as_a_client)
  | 02 ->
      let len = Bytes.index_from buf off '\000' in
      let len = len - off in
      let str = Bytes.sub_string buf off len in
      Ok (len + 1, `Server_identity str)
  | 03 ->
      let len = Bytes.index_from buf off '\000' in
      let len = len - off in
      let str = Bytes.sub_string buf off len in
      Ok (len + 1, `Client_identity str)
  | 04 -> (
      let len = Bytes.index_from buf (off + 4 + 34) '\000' in
      let len = len - (off + 4 + 34) in
      match
        ( of_hex ~off ~len:4 buf,
          Spoke.public_of_string (Bytes.sub_string buf (off + 4) 34) )
      with
      | Ok uid, Ok public ->
          let str = Bytes.sub_string buf (off + 4 + 34) len in
          Ok (4 + 34 + len + 1, `New_server (uid, public, str))
      | Error _, _ -> Error `Invalid_new_server
      | _, Error err -> Error err)
  | 05 -> Ok (64, `Client_validator (Bytes.sub_string buf off 64))
  | 06 ->
      let _Y = Bytes.sub_string buf off 32 in
      let server_validator = Bytes.sub_string buf (off + 32) 64 in
      Ok (96, `Y_and_server_validator (_Y, server_validator))
  | 07 ->
      let len = Bytes.index_from buf (off + 32) '\000' in
      let len = len - (off + 32) in
      let _X = Bytes.sub_string buf off 32 in
      let client_identity = Bytes.sub_string buf (off + 32) len in
      Ok (32 + len + 1, `X_and_client_identity (_X, client_identity))
  | 08 -> Ok (0, `Agreement)
  | 09 -> (
      match of_hex ~off ~len:4 buf with
      | Ok uid -> Ok (4, `Closed uid)
      | Error _ -> Error `Invalid_uid)
  | 10 -> Ok (0, `Accepted)
  | 11 -> Ok (0, `Refused)
  | 12 -> (
      match (Bytes.get buf off, of_hex ~off:(off + 1) ~len:4 buf) with
      | 'C', Ok uid -> Ok (5, `Relay_failure (`Invalid_client uid))
      | 'S', Ok uid -> Ok (5, `Relay_failure (`Invalid_server uid))
      | 'H', Ok uid -> Ok (5, `Relay_failure (`No_handshake_with uid))
      | _, Ok _ -> Error (`Invalid_packet pkt)
      | _, Error _ -> Error `Invalid_uid)
  | 13 -> (
      match Bytes.get buf off with
      | '\000' -> Ok (1, `Spoke_failure `Point_is_not_on_prime_order_subgroup)
      | '\001' -> Ok (1, `Spoke_failure `Invalid_client_validator)
      | '\002' -> Ok (1, `Spoke_failure `Invalid_server_validator)
      | '\003' -> Ok (1, `Spoke_failure `Invalid_public_packet)
      | '\004' -> Ok (1, `Spoke_failure `Invalid_secret_packet)
      | _ -> Error (`Invalid_packet pkt))
  | 14 -> Ok (0, `Done)
  | 15 -> Ok (0, `Timeout)
  | _ -> Error (`Invalid_packet pkt)

let recv ctx =
  let k ctx =
    let uid = of_hex_exn ~off:ctx.ic_pos ~len:4 ctx.ic_buffer in
    let pkt = of_hex_exn ~off:(ctx.ic_pos + 4) ~len:2 ctx.ic_buffer in
    match packet_of_bytes ~pkt ~off:(ctx.ic_pos + 6) ctx.ic_buffer with
    | Ok (shift, raw) ->
        ctx.ic_pos <- ctx.ic_pos + 6 + shift;
        Done (uid, raw)
    | Error err -> Fail err
  in
  prompt k ctx
