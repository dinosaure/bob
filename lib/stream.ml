(* Copyright (c) 2020 Rizo I. <rizo@odis.io>
   Copyright (c) 2022 Romain Calascibetta <romain.calascibetta@gmail.com>

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
  let file ?offset path =
    let init () =
      Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
      | Error errno ->
          Fmt.failwith "openfile(%a): %s" Bob_fpath.pp path
            (Unix.error_message errno)
      | Ok fd -> (
          match offset with
          | None -> Fiber.return fd
          | Some offset ->
              let _ = Unix.LargeFile.lseek fd offset Unix.SEEK_SET in
              Fiber.return fd)
    in
    let stop fd = Fiber.close fd in
    let pull fd =
      Fiber.read fd >>= function
      | Ok `End -> Fiber.return None
      | Ok (`Data str) -> Fiber.return (Some (str, fd))
      | Error errno ->
          Fmt.failwith "read(%d|%a): %s" (Obj.magic fd) Bob_fpath.pp path
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

  let list lst =
    let pull = function
      | [] -> Fiber.return None
      | x :: r -> Fiber.return (Some (x, r))
    in
    Source { init = Fiber.always lst; pull; stop = Fiber.ignore }

  let fold f r0 (Source src) =
    let rec go r s =
      src.pull s >>= function
      | None -> Fiber.return r
      | Some (a, s') -> f r a >>= fun a -> go a s'
    in
    src.init () >>= fun s0 ->
    Fiber.catch
      (fun () ->
        go r0 s0 >>= fun r ->
        src.stop s0 >>= fun () -> Fiber.return r)
      (fun exn -> src.stop s0 >>= fun () -> raise exn)

  let length src = fold (fun count _ -> Fiber.return (succ count)) 0 src

  let unfold seed pull =
    Source { init = Fiber.always seed; pull; stop = (fun _ -> Fiber.return ()) }

  let iterate ~f x =
    unfold x (fun x -> f x >>= fun acc -> Fiber.return (Some (x, acc)))

  let next (Source src) =
    let go s =
      src.pull s >>| function
      | Some (x, s') -> Some (x, Source { src with init = Fiber.always s' })
      | None -> None
    in
    src.init () >>= fun s0 ->
    Fiber.catch
      (fun () -> go s0)
      (fun exn -> src.stop s0 >>= fun () -> raise exn)

  let prepend v (Source source) =
    let init () = Fiber.return (`Pre v) in
    let pull = function
      | `Pre v -> source.init () >>= fun r -> Fiber.return (Some (v, `Src r))
      | `Src r -> (
          source.pull r >>= function
          | Some (v, r) -> Fiber.return (Some (v, `Src r))
          | None -> Fiber.return None)
    in
    let stop = function `Pre _ -> Fiber.return () | `Src r -> source.stop r in
    Source { init; pull; stop }

  let dispose (Source src) = src.init () >>= src.stop
end

type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's Fiber.t;
      push : 's -> 'a -> 's Fiber.t;
      full : 's -> bool Fiber.t;
      stop : 's -> 'r Fiber.t;
    }
      -> ('a, 'r) sink

let rec full_write ~path fd bstr off len =
  Fiber.write fd bstr ~off ~len >>= function
  | Ok len' when len' - len = 0 -> Fiber.return fd
  | Ok len' -> full_write ~path fd bstr (off + len') (len - len')
  | Error `Closed ->
      Fmt.failwith "Unexpected closed fd %d (%a)" (Obj.magic fd) Bob_fpath.pp
        path
  | Error (`Unix errno) ->
      Fmt.failwith "write(%d|%a): %s" (Obj.magic fd) Bob_fpath.pp path
        (Unix.error_message errno)

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

  let zip (Sink l) (Sink r) =
    let init () =
      l.init () >>= fun l_acc ->
      r.init () >>= fun r_acc -> Fiber.return (l_acc, r_acc)
    in
    let push (l_acc, r_acc) x =
      l.push l_acc x >>= fun l_acc ->
      r.push r_acc x >>= fun r_acc -> Fiber.return (l_acc, r_acc)
    in
    let full (l_acc, r_acc) =
      l.full l_acc >>= fun lb ->
      r.full r_acc >>= fun rb -> Fiber.return (lb || rb)
    in
    let stop (l_acc, r_acc) =
      l.stop l_acc >>= fun l_ret ->
      r.stop r_acc >>= fun r_ret -> Fiber.return (l_ret, r_ret)
    in
    Sink { init; push; full; stop }

  type ('top, 'a, 'b) flat_map =
    | Flat_map_top : 'top -> ('top, 'a, 'b) flat_map
    | Flat_map_sub : {
        init : 'sub;
        push : 'sub -> 'a -> 'sub Fiber.t;
        full : 'sub -> bool Fiber.t;
        stop : 'sub -> 'b Fiber.t;
      }
        -> ('top, 'a, 'b) flat_map

  let flat_map f (Sink top) =
    let init () = top.init () >>| fun top -> Flat_map_top top in
    let push s x =
      match s with
      | Flat_map_sub sub ->
          sub.push sub.init x >>| fun init -> Flat_map_sub { sub with init }
      | Flat_map_top acc -> (
          top.push acc x >>= fun acc' ->
          top.full acc' >>= function
          | false -> Fiber.return (Flat_map_top acc')
          | true ->
              top.stop acc' >>= f >>= fun (Sink sub) ->
              sub.init () >>| fun init ->
              Flat_map_sub
                { init; push = sub.push; full = sub.full; stop = sub.stop })
    in
    let full = function
      | Flat_map_top acc -> top.full acc
      | Flat_map_sub sub -> sub.full sub.init
    in
    let stop = function
      | Flat_map_top acc ->
          top.stop acc >>= f >>= fun (Sink sub) -> sub.init () >>= sub.stop
      | Flat_map_sub sub -> sub.stop sub.init
    in
    Sink { init; push; full; stop }

  let fill v =
    Sink
      {
        init = Fiber.always ();
        push = (fun () _ -> Fiber.return ());
        full = Fiber.always true;
        stop = Fiber.always v;
      }

  type ('a, 'b) race = Both of 'a * 'b | Left of 'a | Right of 'b

  let race (Sink l) (Sink r) =
    let init () =
      l.init () >>= fun l_acc ->
      r.init () >>= fun r_acc -> Fiber.return (Both (l_acc, r_acc))
    in
    let push state x =
      match state with
      | Left _ | Right _ -> Fmt.invalid_arg "One of the sinks is already filled"
      | Both (l_acc, r_acc) -> (
          l.push l_acc x >>= fun l_acc' ->
          r.push r_acc x >>= fun r_acc' ->
          l.full l_acc' >>= fun l_full ->
          r.full r_acc' >>= fun r_full ->
          match (l_full, r_full) with
          | true, _ -> Fiber.return (Right r_acc')
          | false, true -> Fiber.return (Left l_acc')
          | false, false -> Fiber.return (Both (l_acc', r_acc')))
    in
    let full = function
      | Both _ -> Fiber.return false
      | _ -> Fiber.return true
    in
    let stop = function
      | Left l_acc -> l.stop l_acc >>| fun l -> Left l
      | Right r_acc -> r.stop r_acc >>| fun r -> Right r
      | Both (l_acc, r_acc) ->
          l.stop l_acc >>= fun l ->
          r.stop r_acc >>= fun r -> Fiber.return (Both (l, r))
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

  let to_string =
    let init () = Fiber.return (Buffer.create 0x100) in
    let push buf bstr =
      Buffer.add_string buf (bigstring_to_string bstr);
      Fiber.return buf
    in
    let full = Fiber.always false in
    let stop buf = Fiber.return (Buffer.contents buf) in
    Sink { init; push; full; stop }

  let is_full (Sink k) = k.init () >>= k.full

  let push x (Sink k) =
    Sink { k with init = (fun () -> k.init () >>= fun acc -> k.push acc x) }

  let file ?(erase = true) path =
    let init () =
      let flags = Unix.[ O_WRONLY; O_APPEND; O_CREAT ] in
      let flags = if erase then Unix.O_TRUNC :: flags else flags in
      Fiber.openfile path flags 0o644 >>= function
      | Ok fd -> Fiber.return fd
      | Error errno ->
          Fmt.failwith "openfile(%a): %s" Bob_fpath.pp path
            (Unix.error_message errno)
    in
    let stop fd = Fiber.close fd in
    let push fd bstr =
      Log.debug (fun m -> m "Push:");
      Log.debug (fun m ->
          m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (bigstring_to_string bstr));
      full_write ~path fd bstr 0 (Bigarray.Array1.dim bstr)
    in
    let full = Fiber.always false in
    Sink { init; stop; full; push }

  let stdout =
    let init () = Fiber.return () in
    let push () bstr =
      full_write ~path:(Bob_fpath.v "<stdout>") Unix.stdout bstr 0
        (Bigarray.Array1.dim bstr)
      >>= fun _stdout -> Fiber.return ()
    in
    let full = Fiber.always false in
    let stop () = Fiber.return () in
    Sink { init; push; full; stop }

  let first =
    Sink
      {
        init = (fun () -> Fiber.return None);
        push = (fun _ x -> Fiber.return (Some x));
        full =
          (function None -> Fiber.return false | Some _ -> Fiber.return true);
        stop = Fiber.return;
      }
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

  let tap f =
    let flow (Sink k) =
      let push r x = f x >>= fun () -> k.push r x in
      Sink { k with push }
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

  let deflate_zlib ?(len = Stdbob.io_buffer_size) ~q ~w level =
    let flow (Sink k) =
      let init () =
        let encoder = Zl.Def.encoder ~q ~w ~level `Manual `Manual in
        let o = De.bigstring_create len in
        let encoder = Zl.Def.dst encoder o 0 len in
        k.init () >>= fun acc -> Fiber.return (encoder, o, acc)
      in
      let push (encoder, o, acc) i =
        Log.debug (fun m -> m "Deflate:");
        Log.debug (fun m ->
            m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (bigstring_to_string i));
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

  let save_into ?offset path =
    let flow (Sink k) =
      let init () =
        let flags = Unix.[ O_WRONLY; O_CREAT ] in
        let flags =
          if Stdlib.Option.is_some offset then flags else Unix.O_APPEND :: flags
        in
        Fiber.openfile path flags 0o644 >>= function
        | Error errno ->
            Fmt.failwith "openfile(%a): %s" Bob_fpath.pp path
              (Unix.error_message errno)
        | Ok fd -> (
            match offset with
            | Some offset ->
                let _ = Unix.LargeFile.lseek fd offset Unix.SEEK_END in
                k.init () >>= fun acc -> Fiber.return (fd, acc)
            | None -> k.init () >>= fun acc -> Fiber.return (fd, acc))
      in
      let push (fd, acc) bstr =
        full_write ~path fd bstr 0 (Bigarray.Array1.dim bstr) >>= fun fd ->
        k.push acc bstr >>= fun acc -> Fiber.return (fd, acc)
      in
      let full (_, acc) = k.full acc in
      let stop (fd, acc) = Fiber.close fd >>= fun () -> k.stop acc in
      Sink { init; stop; full; push }
    in
    { flow }

  let with_digest :
      type ctx.
      (module Digestif.S with type ctx = ctx) ->
      ctx ref ->
      (Stdbob.bigstring, Stdbob.bigstring) flow =
   fun (module Digestif) ctx ->
    let flow (Sink k) =
      let init () = k.init () in
      let push acc bstr =
        k.push acc bstr >>= fun acc ->
        k.full acc >>= function
        | true -> Fiber.return acc
        | false ->
            Log.debug (fun m -> m "Digest:");
            Log.debug (fun m ->
                m "@[<hov>%a@]"
                  (Hxd_string.pp Hxd.default)
                  (Stdbob.bigstring_to_string bstr));
            ctx := Digestif.feed_bigstring !ctx bstr;
            Fiber.return acc
      in
      let full acc = k.full acc in
      let stop acc = k.stop acc in
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
  let run ~from:(Source src) ~via:{ flow } ~into:snk =
    let (Sink snk) = flow snk in
    let rec loop r s =
      snk.full r >>= function
      | true ->
          snk.stop r >>= fun r' ->
          let leftover =
            Source { src with init = (fun () -> Fiber.return s) }
          in
          Fiber.return (r', Some leftover)
      | false -> (
          src.pull s >>= function
          | Some (x, s') -> snk.push r x >>= fun r' -> loop r' s'
          | None ->
              src.stop s >>= fun () ->
              snk.stop r >>= fun r' -> Fiber.return (r', None))
    in
    snk.init () >>= fun r0 ->
    snk.full r0 >>= function
    | true -> snk.stop r0 >>= fun r' -> Fiber.return (r', Some (Source src))
    | false ->
        Fiber.catch
          (fun () -> src.init ())
          (fun exn -> snk.stop r0 >>= fun _ -> raise exn)
        >>= fun s0 ->
        Fiber.catch
          (fun () -> loop r0 s0)
          (fun exn ->
            src.stop s0 >>= fun () ->
            snk.stop r0 >>= fun _r' -> raise exn)

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
  let tap f t = via (Flow.tap f) t

  let of_fiber f =
    let stream (Sink k) =
      k.init () >>= fun acc -> f () >>= k.push acc >>= k.stop
    in
    { stream }

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
  let iterate ~f x = from (Source.iterate ~f x)

  (* Input & Output *)

  let of_file ?(len = Stdbob.io_buffer_size) path =
    Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
    | Error errno ->
        Fiber.return
          (Error
             (msgf "openfile(%a): %s" Bob_fpath.pp path
                (Unix.error_message errno)))
    | Ok fd ->
        let stream (Sink k) =
          let rec go r =
            k.full r >>= function
            | true ->
                Log.debug (fun m ->
                    m "The given sink is full, stop to read into: %a."
                      Bob_fpath.pp path);
                Fiber.return r
            | false -> (
                Fiber.read ~len fd >>= function
                | Ok `End ->
                    Log.debug (fun m ->
                        m "End of the file: %a" Bob_fpath.pp path);
                    Fiber.return r
                | Ok (`Data bstr) -> k.push r bstr >>= go
                | Error errno ->
                    Fmt.failwith "read(%d:%a): %s" (Obj.magic fd) Bob_fpath.pp
                      path (Unix.error_message errno))
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
            | Ok (`Data bstr) -> k.push r bstr >>= go
            | Error errno ->
                Fmt.failwith "read(<stdin>): %s" (Unix.error_message errno))
      in
      bracket go ~init:k.init ~stop:k.stop
    in
    { stream }

  let to_file path = into (Sink.file path)
  let stdout = into Sink.stdout
  let ( >>= ) x f = flat_map f x
  let ( ++ ) = concat
end
