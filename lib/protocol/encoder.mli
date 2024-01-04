type encoder

val encoder : unit -> encoder

type error = [ `Not_enough_space ]

val pp_error : error Fmt.t

type 'err state =
  | Write of {
      buffer : string;
      off : int;
      len : int;
      continue : int -> 'err state;
    }
  | Error of 'err
  | Done

val write : (encoder -> 'err state) -> pkt:string -> encoder -> 'err state
