type encoder = { payload : bytes; mutable pos : int }
type error = [ `Not_enough_space ]

let pp_error ppf = function
  | `Not_enough_space -> Fmt.string ppf "Not enough space"

type 'err state =
  | Write of {
      buffer : string;
      off : int;
      len : int;
      continue : int -> 'err state;
    }
  | Error of 'err
  | Done

let io_buffer_size = 655536
let encoder () = { payload = Bytes.create io_buffer_size; pos = 0 }

exception Leave of error

let leave (_ : encoder) error = raise (Leave error)

let _safe : (encoder -> ([> error ] as 'err) state) -> encoder -> 'err state =
 fun k encoder ->
  try k encoder with Leave (#error as err) -> Error (err :> 'err)

let flush k0 encoder =
  if encoder.pos > 0 then
    let rec k1 n =
      if n < encoder.pos then
        Write
          {
            buffer = Bytes.unsafe_to_string encoder.payload;
            off = n;
            len = encoder.pos - n;
            continue = (fun m -> k1 (n + m));
          }
      else (
        encoder.pos <- 0;
        k0 encoder)
    in
    k1 0
  else k0 encoder

let write str encoder =
  let max = Bytes.length encoder.payload in
  let go j l encoder =
    let rem = max - encoder.pos in
    let len = min rem l in
    Bytes.unsafe_blit_string str j encoder.payload encoder.pos len;
    encoder.pos <- encoder.pos + len;
    if len < l then leave encoder `Not_enough_space
  in
  go 0 (String.length str) encoder

let write k ~pkt:str encoder =
  let len = String.length str in
  let hdr = Fmt.str "%04x" len in
  (* TODO(dinosaure): optimise... *)
  write hdr encoder;
  write str encoder;
  flush k encoder
