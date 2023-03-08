let src = Logs.Src.create "bob.qe"

module Log = (val Logs.src_log src : Logs.LOG)
open Stdbob

type t = {
  mutable rd_cursor : int;
  mutable wr_cursor : int;
  mutable capacity : int;
  mutable buffer : bigstring;
}

let[@inline always] to_power_of_two v =
  let res = ref (pred v) in
  res := !res lor (!res lsr 1);
  res := !res lor (!res lsr 2);
  res := !res lor (!res lsr 4);
  res := !res lor (!res lsr 8);
  res := !res lor (!res lsr 16);
  succ !res

let create capacity =
  let capacity = to_power_of_two capacity in
  {
    rd_cursor = 0;
    wr_cursor = 0;
    capacity;
    buffer = Bigarray.Array1.create Bigarray.char Bigarray.c_layout capacity;
  }

let[@inline always] size t = t.wr_cursor - t.rd_cursor
let[@inline always] mask t v = v land (t.capacity - 1)
let[@inline always] available t = t.capacity - (t.wr_cursor - t.rd_cursor)
let[@inline always] is_empty t = t.rd_cursor = t.wr_cursor

let grow t want =
  let capacity' = to_power_of_two (max 1 (max want (size t))) in
  if capacity' > Bigarray.Array1.dim t.buffer then (
    let buffer' =
      Bigarray.Array1.create Bigarray.char Bigarray.c_layout capacity'
    in
    let size' = (size [@inlined]) t in
    let rd_cursor = (mask [@inlined]) t t.rd_cursor in
    let len0 = t.capacity - rd_cursor in
    let len1 = size' - len0 in
    if len1 > 0 then (
      bigstring_blit t.buffer ~src_off:rd_cursor buffer' ~dst_off:0 ~len:len0;
      bigstring_blit t.buffer ~src_off:0 buffer' ~dst_off:len0 ~len:len1)
    else
      bigstring_blit t.buffer ~src_off:rd_cursor buffer' ~dst_off:0 ~len:size';
    t.buffer <- buffer';
    t.wr_cursor <- size';
    t.capacity <- capacity';
    t.rd_cursor <- 0)

let compress t =
  let size = size t in
  let rd_cursor = (mask [@inlined]) t t.rd_cursor in
  let len0 = t.capacity - rd_cursor in
  let len1 = size - len0 in
  (* TODO(dinosaure): we probably can optimize this part and avoid
     an allocation in some situations. However, we must be sure about
     the semantic of [blit], is it a [memcpy] or a [memmove]?

     In our case and according to [Stdbob], it's a [memcpy], the overlap
     is not handled so. *)
  (if len1 > 0 then (
     let buffer' =
       Bigarray.Array1.create Bigarray.char Bigarray.c_layout t.capacity
     in
     bigstring_blit t.buffer ~src_off:rd_cursor buffer' ~dst_off:0 ~len:len0;
     bigstring_blit t.buffer ~src_off:0 buffer' ~dst_off:len0 ~len:len1;
     t.buffer <- buffer')
   else
     let buffer' =
       Bigarray.Array1.create Bigarray.char Bigarray.c_layout t.capacity
     in
     bigstring_blit t.buffer ~src_off:rd_cursor buffer' ~dst_off:0 ~len:len0;
     t.buffer <- buffer');
  t.rd_cursor <- 0;
  t.wr_cursor <- size

let push t ?(off = 0) ?len bstr =
  let len =
    match len with Some len -> len | None -> Bigarray.Array1.dim bstr - off
  in
  if (available [@inlined]) t < len then grow t (len + (size [@inlined]) t);
  let wr_cursor = (mask [@inlined]) t t.wr_cursor in
  let len0 = t.capacity - wr_cursor in
  let len1 = len - len0 in
  if len1 > 0 then (
    bigstring_blit bstr ~src_off:off t.buffer ~dst_off:wr_cursor ~len:len0;
    bigstring_blit bstr ~src_off:(off + len0) t.buffer ~dst_off:0 ~len:len1)
  else bigstring_blit bstr ~src_off:off t.buffer ~dst_off:wr_cursor ~len;
  t.wr_cursor <- t.wr_cursor + len

let peek t =
  let len = (size [@inlined]) t in
  if len = 0 then []
  else
    let rd_cursor = (mask [@inlined]) t t.rd_cursor in
    let len0 = t.capacity - rd_cursor in
    let len1 = len - len0 in
    if len1 > 0 then
      [
        bigstring_copy t.buffer ~off:rd_cursor ~len:len0;
        bigstring_copy t.buffer ~off:0 ~len:len1;
      ]
    else [ bigstring_copy t.buffer ~off:rd_cursor ~len ]

let keep t ?(off = 0) ?len bstr =
  let len =
    match len with Some len -> len | None -> Bigarray.Array1.dim bstr - off
  in
  if len > (size [@inlined]) t then
    Fmt.invalid_arg "Qe.keep: not enough byte(s)";
  let rd_cursor = (mask [@inlined]) t t.rd_cursor in
  let len0 = t.capacity - rd_cursor in
  let len1 = len - len0 in
  if len1 > 0 then (
    bigstring_blit t.buffer ~src_off:rd_cursor bstr ~dst_off:off ~len:len0;
    bigstring_blit t.buffer ~src_off:0 bstr ~dst_off:(off + len0) ~len:len1)
  else bigstring_blit t.buffer ~src_off:rd_cursor bstr ~dst_off:off ~len

let shift t len =
  if len > (size [@inlined]) t then
    Fmt.invalid_arg "Qe.shift: not enough byte(s)";
  t.rd_cursor <- t.rd_cursor + len
