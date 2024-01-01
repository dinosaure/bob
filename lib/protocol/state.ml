type ('a, 'err) t =
  | Read of { buffer : bytes; off : int; len : int; continue : [ `End | `Len of int ] -> ('a, 'err) t }
  | Write of { buffer : string; off : int; len : int; continue : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err
