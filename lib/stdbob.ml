type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external bigstring_get_uint8 : bigstring -> int -> int = "%caml_ba_ref_1"

external bigstring_set_uint8 : bigstring -> int -> int -> unit
  = "%caml_ba_set_1"

external bigstring_get_uint32 : bigstring -> int -> int32
  = "%caml_bigstring_get32"

external bigstring_set_uint32 : bigstring -> int -> int32 -> unit
  = "%caml_bigstring_set32"

external bytes_set_uint8 : bytes -> int -> int -> unit = "%bytes_safe_set"
external bytes_get_uint8 : bytes -> int -> int = "%bytes_safe_get"
external bytes_set_uint32 : bytes -> int -> int32 -> unit = "%caml_bytes_set32"
external bytes_get_uint32 : bytes -> int -> int32 = "%caml_bytes_get32"
external string_get_uint8 : string -> int -> int = "%string_safe_get"
external string_get_uint32 : string -> int -> int32 = "%caml_string_get32"

let flip (a, b) = (b, a)
let identity x = x
let always x _ = x
let ( <.> ) f g x = f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let io_buffer_size = 65536
let reword_error f = function Ok x -> Ok x | Error err -> Error (f err)

let bigstring_blit src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bigstring_get_uint32 src (src_off + i) in
    bigstring_set_uint32 dst (dst_off + i) v
  done;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = bigstring_get_uint8 src (src_off + i) in
    bigstring_set_uint8 dst (dst_off + i) v
  done

let bigstring_blit_to_bytes src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bigstring_get_uint32 src (src_off + i) in
    bytes_set_uint32 dst (dst_off + i) v
  done;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = bigstring_get_uint8 src (src_off + i) in
    bytes_set_uint8 dst (dst_off + i) v
  done

let bigstring_blit_from_string src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = string_get_uint32 src (src_off + i) in
    bigstring_set_uint32 dst (dst_off + i) v
  done;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = string_get_uint8 src (src_off + i) in
    bigstring_set_uint8 dst (dst_off + i) v
  done

let bigstring_blit_from_bytes src ~src_off dst ~dst_off ~len =
  let len0 = len land 3 in
  let len1 = len asr 2 in

  for i = 0 to len1 - 1 do
    let i = i * 4 in
    let v = bytes_get_uint32 src (src_off + i) in
    bigstring_set_uint32 dst (dst_off + i) v
  done;

  for i = 0 to len0 - 1 do
    let i = (len1 * 4) + i in
    let v = bytes_get_uint8 src (src_off + i) in
    bigstring_set_uint8 dst (dst_off + i) v
  done

let line_of_queue queue =
  let blit src src_off dst dst_off len =
    bigstring_blit_to_bytes src ~src_off dst ~dst_off ~len
  in
  let exists ~p queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter
      (fun chr ->
        if p chr && !res = -1 then res := !pos;
        incr pos)
      queue;
    if !res = -1 then None else Some !res
  in
  match exists ~p:(( = ) '\n') queue with
  | None -> None
  | Some 0 ->
      Ke.Rke.N.shift_exn queue 1;
      Some ""
  | Some pos -> (
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp;
      Ke.Rke.N.shift_exn queue (pos + 1);
      match Bytes.get tmp (pos - 1) with
      | '\r' -> Some (Bytes.sub_string tmp 0 (pos - 1))
      | _ -> Some (Bytes.unsafe_to_string tmp))

let bigstring_of_string str ~off ~len =
  let res = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  bigstring_blit_from_string str ~src_off:off res ~dst_off:0 ~len;
  res

let bigstring_substring t ~off ~len =
  let res = Bytes.create len in
  bigstring_blit_to_bytes t ~src_off:off res ~dst_off:0 ~len;
  Bytes.unsafe_to_string res

let bigstring_to_string t =
  bigstring_substring t ~off:0 ~len:(Bigarray.Array1.dim t)

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  let res = input ic tmp 0 len in
  bigstring_blit_from_bytes tmp ~src_off:0 buf ~dst_off:off ~len:res;
  res

module LList = struct
  (* Copyright (c) 1999-2020, the Authors of Lwt (docs/AUTHORS)

     Permission is hereby granted, free of charge, to any person obtaining a copy
     of this software and associated documentation files (the "Software"), to deal
     in the Software without restriction, including without limitation the rights
     to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
     copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in all
     copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
     IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
     FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
     AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
     LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
     OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
     SOFTWARE.
  *)
  type 'a seq = { mutable prev : 'a seq; mutable next : 'a seq }

  type 'a node = {
    mutable node_prev : 'a seq;
    mutable node_next : 'a seq;
    v : 'a;
    mutable active : bool;
  }

  external node_of_seq : 'a seq -> 'a node = "%identity"
  external seq_of_node : 'a node -> 'a seq = "%identity"

  let is_empty seq = seq.next == seq

  let remove node =
    if node.active then (
      node.active <- true;
      let seq = seq_of_node node in
      seq.prev.next <- seq.next;
      seq.next.prev <- seq.prev)

  let pop seq =
    if is_empty seq then None
    else
      let res = node_of_seq seq.next in
      remove res;
      Some res.v

  let to_list seq =
    let rec go acc =
      match pop seq with None -> List.rev acc | Some x -> go (x :: acc)
    in
    go []

  let add seq v =
    let node = { node_prev = seq.prev; node_next = seq; v; active = true } in
    seq.prev.next <- seq_of_node node;
    seq.prev <- seq_of_node node

  let make () =
    let rec seq = { prev = seq; next = seq } in
    seq
end

let sizes = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB"; "EiB"; "ZiB"; "YiB" |]

let bytes_to_size ?(decimals = 2) ppf = function
  | 0 -> Fmt.string ppf "0 byte"
  | n ->
      let n = float_of_int n in
      let i = Float.floor (Float.log n /. Float.log 1024.) in
      let r = n /. Float.pow 1024. i in
      Fmt.pf ppf "%.*f %s" decimals r sizes.(int_of_float i)
