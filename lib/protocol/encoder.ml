type encoder = { payload : bytes; mutable pos : int }

type error = [ `Not_enough_space ]

type 'err state =
  | Write of { buffer : string; off : int; len : int; continue : int -> 'err state }
  | Error of 'err
  | Done

let io_buffer_size = 655536
let encoder () = { payload= Bytes.create io_buffer_size; pos= 0 }

exception Leave of error

let leave (_ : encoder) error = raise (Leave error)

let safe
  : (encoder -> ([> error ] as 'err) state) -> encoder -> 'err state
  = fun k encoder ->
    try k encoder
    with Leave (#error as err) -> Error (err :> 'err)

let flush k0 encoder =
  if encoder.pos > 0
  then
  let rec k1 n =
    if n < encoder.pos
    then Write { buffer= Bytes.unsafe_to_string encoder.payload; off= n; len= encoder.pos - n; continue= (fun m -> k1 (n + m)) }
    else begin encoder.pos <- 0; k0 encoder end in
  k1 0
  else k0 encoder

let write str encoder =
  let max = Bytes.length encoder.payload in
  let go j l encoder =
    let rem = max - encoder.pos in
    let len = min rem l in
    Bytes.unsafe_blit_string str j encoder.payloa encoder.pos len;
    encoder.pos <- encoder.pos + len;
    if len < le then leave_with encoder `Not_enough_space in
  go 0 (String.length str) encoder
