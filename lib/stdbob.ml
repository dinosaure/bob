let flip (a, b) = (b, a)
let rev f x y = f y x
let identity x = x
let always x _ = x
let ( <.> ) f g x = f (g x)
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let io_buffer_size = 65536 (* 0x10000 & = De.io_buffer_size *)
let reword_error f = function Ok x -> Ok x | Error err -> Error (f err)
let never _ = assert false
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

external reraise : exn -> 'a = "%reraise"

let line_of_queue queue =
  let blit src src_off dst dst_off len =
    Bstr.blit_to_bytes src ~src_off dst ~dst_off ~len
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
  [@@@warning "-69"]

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

let add str ~start ~stop acc =
  if start = stop then "" :: acc else String.sub str start (stop - start) :: acc

let cuts ~sep str =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "Stdbob.cuts";
  let str_len = String.length str in
  let max_sep_idx = sep_len - 1 in
  let max_str_idx = str_len - sep_len in
  let rec check_sep start i k acc =
    if k > max_sep_idx then
      let new_start = i + sep_len in
      scan new_start new_start (add str ~start ~stop:i acc)
    else if str.[i + k] = sep.[k] then check_sep start i (k + 1) acc
    else scan start (i + 1) acc
  and scan start i acc =
    if i > max_str_idx then
      if start = 0 then [ str ] else List.rev (add str ~start ~stop:str_len acc)
    else if str.[i] = sep.[0] then check_sep start i 1 acc
    else scan start (i + 1) acc
  in
  scan 0 0 []
