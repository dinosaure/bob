type decoder

val decoder : unit -> decoder

type ('v, 'err) state =
  | Done of 'v
  | Read of {
      buffer : Bytes.t;
      off : int;
      len : int;
      continue : [ `End | `Len of int ] -> ('v, 'err) state;
    }
  | Error of 'err info

and 'err info = { error : 'err; buffer : Bytes.t; committed : int }

type error =
  [ `End_of_input | `Not_enough_space | `Invalid_packet | `Partial_packet ]

val return : 'v -> decoder -> ('v, [> error ]) state

val prompt :
  (decoder -> ('v, ([> error ] as 'err)) state) -> decoder -> ('v, 'err) state

val peek_packet : decoder -> string
