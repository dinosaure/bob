(* Copyright (c) 2020 Rizo I. <rizo@odis.io>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

   XXX(dinosaure): stream + fiber. The [Lwt_stream] is too bad and we have
   an opportunity to do one on our own way. Thanks to rizo for the start.
   Again, the limitation is the higher kind polymorphism where we need to
   modify the code to put the [Fiber.t] into all retunred values.
*)

open Stdbob
open Fiber

let src = Logs.Src.create "bob.stream"

module Log = (val Logs.src_log src : Logs.LOG)

type 'a source =
  | Source : {
      init : unit -> 's Fiber.t;
      pull : 's -> ('a * 's) option Fiber.t;
      stop : 's -> unit Fiber.t;
    }
      -> 'a source

module Source = struct
  let file path =
    let init () =
      Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
      | Ok fd -> Fiber.return fd
      | Error errno ->
          Fmt.failwith "openfile(%a): %s" Fpath.pp path
            (Unix.error_message errno)
    in
    let stop fd = Fiber.close fd in
    let pull fd =
      Fiber.read fd >>= function
      | Ok `End -> Fiber.return None
      | Ok (`Data str) -> Fiber.return (Some (str, fd))
      | Error errno ->
          Fmt.failwith "read(%d|%a): %s" (Obj.magic fd) Fpath.pp path
            (Unix.error_message errno)
    in
    Source { init; stop; pull }

  let array arr =
    let len = Array.length arr in
    let pull i =
      if i >= len then Fiber.return None
      else Fiber.return (Some (arr.(i), succ i))
    in
    Source { init = Fiber.always 0; pull; stop = Fiber.ignore }
end

type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's Fiber.t;
      push : 's -> 'a -> 's Fiber.t;
      full : 's -> bool Fiber.t;
      stop : 's -> 'r Fiber.t;
    }
      -> ('a, 'r) sink

module Sink = struct
  let make ~init ~push ?(full = Fiber.always false) ~stop () =
    Sink { init; push; full; stop }

  let list =
    let init () = Fiber.return [] in
    let push acc x = Fiber.return (x :: acc) in
    let full _ = Fiber.return false in
    let stop acc = Fiber.return (List.rev acc) in
    Sink { init; push; full; stop }

  let string =
    let init () = Fiber.return (Buffer.create 128) in
    let push buf str =
      Buffer.add_string buf str;
      Fiber.return buf
    in
    let full = Fiber.always false in
    let stop buf = Fiber.return (Buffer.contents buf) in
    Sink { init; push; full; stop }

  let array =
    let init () = Fiber.return [] in
    let push acc x = Fiber.return (x :: acc) in
    let full _ = Fiber.return false in
    let stop acc =
      match acc with
      | [] -> Fiber.return [||]
      | [ x ] -> Fiber.return [| x |]
      | _ -> Fiber.return (Array.of_list acc)
    in
    Sink { init; push; full; stop }

  let bigstring =
    let bigstring_blit src src_off dst dst_off len =
      Stdbob.bigstring_blit src ~src_off dst ~dst_off ~len
    in
    let init () = Fiber.return (Ke.Rke.create ~capacity:128 Bigarray.char) in
    let push ke bstr =
      Ke.Rke.N.push ke ~blit:bigstring_blit ~length:Bigarray.Array1.dim bstr;
      Fiber.return ke
    in
    let full = Fiber.always false in
    let stop ke =
      match Ke.Rke.N.peek ke with
      | [] -> Fiber.return De.bigstring_empty
      | [ x ] -> Fiber.return x
      | _ ->
          Log.err (fun m ->
              m "Too many elements returned by our internal queue.");
          assert false
    in
    Sink { init; push; full; stop }

  let is_full (Sink k) = k.init () >>= k.full

  let push x (Sink k) =
    Sink { k with init = (fun () -> k.init () >>= fun acc -> k.push acc x) }

  let rec until ~path fd bstr off len =
    Fiber.write fd bstr ~off ~len >>= function
    | Ok len' when len' - len = 0 -> Fiber.return fd
    | Ok len' -> until ~path fd bstr (off + len') (len - len')
    | Error `Closed ->
        Fmt.failwith "Unexpected closed fd %d (%a)" (Obj.magic fd) Fpath.pp path
    | Error (`Unix errno) ->
        Fmt.failwith "write(%d|%a): %s" (Obj.magic fd) Fpath.pp path
          (Unix.error_message errno)

  let file path =
    let init () =
      Fiber.openfile path Unix.[ O_WRONLY; O_APPEND; O_CREAT ] 0o644
      >>= function
      | Ok fd -> Fiber.return fd
      | Error errno ->
          Fmt.failwith "openfile(%a): %s" Fpath.pp path
            (Unix.error_message errno)
    in
    let stop fd = Fiber.close fd in
    let push fd bstr = until ~path fd bstr 0 (Bigarray.Array1.dim bstr) in
    let full = Fiber.always false in
    Sink { init; stop; full; push }

  let stdout =
    let init () = Fiber.return () in
    let push () bstr =
      until ~path:(Fpath.v "<stdout>") Unix.stdout bstr 0
        (Bigarray.Array1.dim bstr)
      >>= fun _stdout -> Fiber.return ()
    in
    let full = Fiber.always false in
    let stop () = Fiber.return () in
    Sink { init; push; full; stop }
end

type ('a, 'b) flow = { flow : 'r. ('b, 'r) sink -> ('a, 'r) sink } [@@unboxed]

module Flow = struct
  let identity = { flow = identity }
  let compose { flow = f } { flow = g } = { flow = (fun sink -> f (g sink)) }
  let ( << ) a b = compose a b
  let ( >> ) b a = compose a b

  let map f =
    let flow (Sink k) =
      let push r x = f x >>= k.push r in
      Sink { k with push }
    in
    { flow }

  let filter_map f =
    let flow (Sink k) =
      let push r x =
        f x >>= function Some x' -> k.push r x' | None -> Fiber.return r
      in
      Sink { k with push }
    in
    { flow }

  (* Buffering *)

  let flush_if_full ~push ~acc ke capacity =
    if Ke.Rke.Weighted.length ke = capacity then (
      let vs = Ke.Rke.Weighted.N.peek ke in
      let rec go acc = function
        | [] -> Fiber.return acc
        | x :: r -> push acc x >>= fun acc -> go acc r
      in
      go acc vs >>= fun acc ->
      Ke.Rke.Weighted.N.shift_exn ke (Ke.Rke.Weighted.length ke);
      Fiber.return acc)
    else Fiber.return acc

  let flush ~push ~acc ke capacity =
    if Ke.Rke.Weighted.available ke = capacity then Fiber.return acc
    else
      let vs = Ke.Rke.Weighted.N.peek ke in
      let rec go acc = function
        | [] -> Fiber.return acc
        | x :: r -> push acc x >>= fun acc -> go acc r
      in
      go acc vs >>= fun acc ->
      Ke.Rke.Weighted.N.shift_exn ke (Ke.Rke.Weighted.length ke);
      Fiber.return acc

  let rec push_until_empty ~push ~acc ke capacity str ~off ~len =
    flush_if_full ~push ~acc ke capacity >>= fun acc ->
    let max = min (Ke.Rke.Weighted.available ke) len in
    let _ =
      Ke.Rke.Weighted.N.push_exn ke ~blit:bigstring_blit_from_string
        ~length:String.length str ~off ~len:max
    in
    if len - max = 0 then Fiber.return (ke, capacity, acc)
    else
      push_until_empty ~push ~acc ke capacity str ~off:(off + max)
        ~len:(len - max)

  and bigstring_blit_from_string src src_off dst dst_off len =
    Stdbob.bigstring_blit_from_string src ~src_off dst ~dst_off ~len

  let bigbuffer capacity =
    let flow (Sink k) =
      let init () =
        k.init () >>= fun acc ->
        let ke, capacity = Ke.Rke.Weighted.create ~capacity Bigarray.char in
        Fiber.return (ke, capacity, acc)
      in
      let push (ke, capacity, acc) str =
        push_until_empty ~push:k.push ~acc ke capacity str ~off:0
          ~len:(String.length str)
      in
      let full (_, _, acc) = k.full acc in
      let stop (ke, capacity, acc) =
        flush ~push:k.push ~acc ke capacity >>= k.stop
      in
      Sink { init; stop; full; push }
    in
    { flow }

  (* Zlib *)

  let rec deflate_zlib_until_end ~push ~acc encoder o =
    match Zl.Def.encode encoder with
    | `Await _ -> assert false
    | `Flush encoder ->
        let len = Bigarray.Array1.dim o - Zl.Def.dst_rem encoder in
        let encoder = Zl.Def.dst encoder o 0 (Bigarray.Array1.dim o) in
        push acc (Bigarray.Array1.sub o 0 len) >>= fun acc ->
        deflate_zlib_until_end ~push ~acc encoder o
    | `End encoder ->
        let len = Bigarray.Array1.dim o - Zl.Def.dst_rem encoder in
        push acc (Bigarray.Array1.sub o 0 len)

  let rec deflate_zlib_until_await ~push ~acc encoder o =
    match Zl.Def.encode encoder with
    | `Await encoder -> Fiber.return (encoder, o, acc)
    | `Flush encoder ->
        let len = Bigarray.Array1.dim o - Zl.Def.dst_rem encoder in
        let encoder = Zl.Def.dst encoder o 0 (Bigarray.Array1.dim o) in
        push acc (Bigarray.Array1.sub o 0 len) >>= fun acc ->
        deflate_zlib_until_await ~push ~acc encoder o
    | `End _ -> assert false

  let deflate_zlib ~q ~w ~level =
    let flow (Sink k) =
      let init () =
        let encoder = Zl.Def.encoder ~q ~w ~level `Manual `Manual in
        let o = De.bigstring_create io_buffer_size in
        let encoder = Zl.Def.dst encoder o 0 io_buffer_size in
        k.init () >>= fun acc -> Fiber.return (encoder, o, acc)
      in
      let push (encoder, o, acc) i =
        deflate_zlib_until_await ~push:k.push ~acc
          (Zl.Def.src encoder i 0 (Bigarray.Array1.dim i))
          o
      in
      let full (_, _, acc) = k.full acc in
      let stop (encoder, o, acc) =
        deflate_zlib_until_end ~push:k.push ~acc
          (Zl.Def.src encoder De.bigstring_empty 0 0)
          o
        >>= k.stop
      in
      Sink { init; stop; full; push }
    in
    { flow }
end

type 'a stream = { stream : 'r. ('a, 'r) sink -> 'r Fiber.t } [@@unboxed]

let bracket :
    init:(unit -> 's Fiber.t) ->
    stop:('s -> 'r Fiber.t) ->
    ('s -> 's Fiber.t) ->
    'r Fiber.t =
 fun ~init ~stop f ->
  init () >>= fun acc ->
  Fiber.catch
    (fun () -> f acc >>= stop)
    (fun exn -> stop acc >>= fun _ -> raise exn)

module Stream = struct
  (* Composition *)

  let into sink t = t.stream sink

  let via { flow } t =
    let stream sink = into (flow sink) t in
    { stream }

  let from (Source src) =
    let stream (Sink k) =
      let rec go r s =
        k.full r >>= function
        | true -> k.stop r
        | false -> (
            src.pull s >>= function
            | None -> src.stop s >>= fun () -> k.stop r
            | Some (x, s') -> k.push r x >>= fun r -> go r s')
      in
      k.init () >>= fun r0 ->
      k.full r0 >>= function
      | true -> k.stop r0
      | false ->
          Fiber.catch
            (fun () -> src.init ())
            (fun exn -> k.stop r0 >>= fun _ -> raise exn)
          >>= fun s0 ->
          Fiber.catch
            (fun () -> go r0 s0)
            (fun exn ->
              src.stop s0 >>= fun () ->
              k.stop r0 >>= fun _ -> raise exn)
    in
    { stream }

  (* Basics *)

  let concat a b =
    let stream (Sink k) =
      let stop r =
        k.full r >>= function
        | true -> k.stop r
        | false -> b.stream (Sink { k with init = (fun () -> Fiber.return r) })
      in
      a.stream (Sink { k with stop })
    in
    { stream }

  let flat_map f t =
    let stream (Sink k) =
      let push r x =
        f x >>= fun { stream } ->
        stream
          (Sink
             {
               k with
               init = (fun () -> Fiber.return r);
               stop = (fun r -> Fiber.return r);
             })
      in
      t.stream (Sink { k with push })
    in
    { stream }

  let empty =
    let stream (Sink k) = k.init () >>= k.stop in
    { stream }

  let singleton v =
    let stream (Sink k) = k.init () >>= fun acc -> k.push acc v >>= k.stop in
    { stream }

  let double a b =
    let stream (Sink k) =
      k.init () >>= fun acc ->
      k.push acc a >>= fun acc -> k.push acc b >>= k.stop
    in
    { stream }

  let map f t = via (Flow.map f) t
  let filter_map f t = via (Flow.filter_map f) t

  let of_iter iter =
    let exception Stop in
    let stream (Sink k) =
      let go r =
        k.full r >>= function
        | true -> Fiber.return r
        | false ->
            let acc = ref r in
            Fiber.catch
              (fun () ->
                iter (fun x ->
                    k.push !acc x >>= fun acc' ->
                    acc := acc';
                    k.full !acc >>= function
                    | true -> raise Stop
                    | false -> Fiber.return ())
                >>= fun () -> Fiber.return !acc)
              (function Stop -> Fiber.return !acc | exn -> raise exn)
      in
      bracket go ~init:k.init ~stop:k.stop
    in
    { stream }

  let to_list t = into Sink.list t

  let of_list lst =
    let stream (Sink k) =
      let rec go s r =
        k.full r >>= function
        | true -> Fiber.return r
        | false -> (
            match s with
            | [] -> Fiber.return r
            | x :: s' -> k.push r x >>= go s')
      in
      bracket (go lst) ~init:k.init ~stop:k.stop
    in
    { stream }

  let to_array stream = into Sink.array stream
  let of_array arr = from (Source.array arr)
  let to_bigstring stream = into Sink.bigstring stream
  let to_string stream = into Sink.string stream

  (* Input & Output *)

  let of_file path =
    Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
    | Error errno ->
        Fiber.return
          (Error
             (msgf "openfile(%a): %s" Fpath.pp path (Unix.error_message errno)))
    | Ok fd ->
        let stream (Sink k) =
          let rec go r =
            k.full r >>= function
            | true -> Fiber.return r
            | false -> (
                Fiber.read fd >>= function
                | Ok `End -> Fiber.return r
                | Ok (`Data str) -> k.push r str >>= go
                | Error errno ->
                    Fmt.failwith "read(%d:%a): %s" (Obj.magic fd) Fpath.pp path
                      (Unix.error_message errno))
          in
          let stop r = Fiber.close fd >>= fun () -> k.stop r in
          bracket go ~init:k.init ~stop
        in
        Fiber.return (Ok { stream })

  let stdin =
    let stream (Sink k) =
      let rec go r =
        k.full r >>= function
        | true -> Fiber.return r
        | false -> (
            Fiber.read Unix.stdin >>= function
            | Ok `End -> Fiber.return r
            | Ok (`Data str) -> k.push r str >>= go
            | Error errno ->
                Fmt.failwith "read(<stdin>): %s" (Unix.error_message errno))
      in
      bracket go ~init:k.init ~stop:k.stop
    in
    { stream }

  let to_file path = into (Sink.file path)
  let stdout = into Sink.stdout

  module Infix = struct
    let ( >>= ) x f = flat_map f x
    let ( ++ ) = concat
  end
end
