let src = Logs.Src.create "bob.carton"

module Log = (val Logs.src_log src : Logs.LOG)
open Stdbob
open Fiber
module SHA1 = Digestif.SHA1

module Window = struct
  type 'meta t = {
    arr : 'meta Cartonnage.Source.t array;
    mutable rd_pos : int;
    mutable wr_pos : int;
  }

  let make () =
    { arr = Array.make 0x100 (Obj.magic ()); rd_pos = 0; wr_pos = 0 }

  (* let is_empty { rd_pos; wr_pos; _ } = rd_pos = wr_pos *)
  let is_full { rd_pos; wr_pos; arr } = wr_pos - rd_pos = Array.length arr
end

let should_we_apply ~source entry =
  let open Cartonnage in
  let size_guessed =
    match Target.patch entry with
    | None -> Target.length entry / 3
    | Some patch -> Patch.length patch / 3
  in
  if Source.length source < Target.length entry then false
  else
    let diff = Source.length source - Target.length entry in
    diff < size_guessed

let apply ~load ~window:t entry =
  let len = t.Window.wr_pos - t.Window.rd_pos in
  let msk = Array.length t.Window.arr - 1 in
  let uid = Cartonnage.Target.uid entry
  and meta = Cartonnage.Target.meta entry in
  let target = Lazy.from_fun (fun () -> load uid meta) in
  let rec go i =
    if i < len then
      let source = t.Window.arr.((t.Window.rd_pos + i) land msk) in
      if Cartonnage.Source.depth source < 50 && should_we_apply ~source entry
      then (
        Lazy.force target >>= fun target ->
        Cartonnage.Target.diff entry ~source ~target;
        go (succ i))
      else go (succ i)
    else Fiber.return ()
  in
  go 0 >>= fun () ->
  if Lazy.is_val target || Cartonnage.Target.depth entry == 1 then
    Lazy.force target >>| Stdlib.Option.some
  else Fiber.return None

let append ~window:t source =
  let open Window in
  let msk = Array.length t.arr - 1 in
  match Array.length t.arr - (t.wr_pos - t.rd_pos) with
  | 0 ->
      t.arr.(t.rd_pos land msk) <- source;
      t.rd_pos <- t.rd_pos + 1;
      t.wr_pos <- t.wr_pos + 1
  | _ ->
      t.arr.(t.wr_pos land msk) <- source;
      t.wr_pos <- t.wr_pos + 1

let delta ~reporter:_ ~load entries =
  let windows = Array.init 4 (fun _ -> Window.make ()) in
  let fn entry =
    let entry = Cartonnage.Target.make entry in
    let k = Carton.Kind.to_int (Cartonnage.Target.kind entry) in
    let window = windows.(k) in
    apply ~load ~window entry >>= fun target ->
    (match (target, not (Window.is_full windows.(k))) with
    | None, false -> Fiber.return ()
    | None, true ->
        if Cartonnage.Target.depth entry < 50 then (
          let uid = Cartonnage.Target.uid entry
          and meta = Cartonnage.Target.meta entry in
          load uid meta >>= fun target ->
          let source = Cartonnage.Target.to_source entry ~target in
          append ~window source;
          Fiber.return ())
        else Fiber.return ()
    | Some target, _ ->
        (if Cartonnage.Target.depth entry < 50 then
           let source = Cartonnage.Target.to_source entry ~target in
           append ~window source);
        Fiber.return ())
    >>| fun () -> entry
  in
  Stream.Stream.map fn entries

type cartonnage = {
  ctx : SHA1.ctx;
  cursor : int;
  buffers : Cartonnage.buffers;
  where : (Carton.Uid.t, int) Hashtbl.t;
}

let rec go t ~push encoder acc =
  let dst = t.buffers.o in
  match Cartonnage.Encoder.encode ~o:dst encoder with
  | `Flush (encoder, len) ->
      let ctx = SHA1.feed_bigstring t.ctx dst ~off:0 ~len in
      let encoder =
        Cartonnage.Encoder.dst encoder dst 0 (Bigarray.Array1.dim dst)
      in
      let t = { t with ctx; cursor = t.cursor + len } in
      push acc (Bigarray.Array1.sub dst 0 len) >>= fun acc ->
      go t ~push encoder acc
  | `End -> Fiber.return (t, acc)

let pack ~reporter:_ ?level ~length load =
  let flow (Stream.Sink k) =
    let init () =
      Log.debug (fun m -> m "Start to encode a PACK file.");
      let o = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x1000
      and i = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x1000
      and q = De.Queue.create 0x1000
      and w = De.Lz77.make_window ~bits:15 in
      let buffers = Cartonnage.{ o; i; q; w } in
      let where = Hashtbl.create 0x100 in
      k.init () >>= fun acc ->
      let hdr = Bytes.create 12 in
      let ctx = SHA1.empty in
      Bytes.set_int32_be hdr 0 0x5041434bl;
      Bytes.set_int32_be hdr 4 2l;
      Bytes.set_int32_be hdr 8 (Int32.of_int length);
      let ctx = SHA1.feed_bytes ctx hdr in
      let hdr =
        bigstring_of_string (Bytes.unsafe_to_string hdr) ~off:0 ~len:12
      in
      k.push acc hdr >>= fun acc ->
      Fiber.return ({ ctx; cursor = 12; buffers; where }, acc)
    in
    let push (t, acc) entry =
      let uid = Cartonnage.Target.uid entry in
      let meta = Cartonnage.Target.meta entry in
      load uid meta >>= fun target ->
      let buffers = t.buffers in
      let where = Hashtbl.find_opt t.where in
      let _hdr_len, encoder =
        Cartonnage.encode ?level ~buffers ~where entry ~target ~cursor:t.cursor
      in
      Hashtbl.add t.where (Cartonnage.Target.uid entry) t.cursor;
      go t ~push:k.push encoder acc
    in
    let full (_, acc) = k.full acc in
    let stop (t, acc) =
      let hash = SHA1.get t.ctx in
      Log.debug (fun m -> m "Hash of the PACK file: %a" SHA1.pp hash);
      let hash = SHA1.to_raw_string hash in
      let hash = bigstring_of_string hash ~off:0 ~len:(String.length hash) in
      k.push acc hash >>= k.stop
    in
    Stream.Sink { init; stop; full; push }
  in
  { Stream.flow }

type base = { value : Carton.Value.t; uid : Carton.Uid.t; depth : int }

let identify (Carton.Identify gen) ~kind ~len bstr =
  let ctx = gen.Carton.First_pass.init kind (Carton.Size.of_int_exn len) in
  let ctx = gen.Carton.First_pass.feed (Bigarray.Array1.sub bstr 0 len) ctx in
  gen.Carton.First_pass.serialize ctx

(* NOTE(dinosaure): here we directly Carton functions which don't yield.
   but that's fine. We want to implement a client, the question about
   the availability of our application is not an issue. *)

let rec resolve_tree ~on t oracle matrix ~(base : base) = function
  | [||] -> Fiber.return ()
  | [| cursor |] ->
      let value = Carton.of_offset_with_source t base.value ~cursor in
      let len = Carton.Value.length value
      and bstr = Carton.Value.bigstring value
      and kind = Carton.Value.kind value in
      let uid = identify oracle.Carton.identify ~kind ~len bstr
      and pos = oracle.Carton.where ~cursor
      and crc = oracle.Carton.checksum ~cursor
      and depth = succ base.depth in
      on value uid;
      matrix.(pos) <-
        Carton.Resolved_node
          { cursor; uid; crc; kind; depth; parent = base.uid };
      let children = oracle.Carton.children ~cursor ~uid in
      let children = Array.of_list children in
      let value = Carton.Value.flip value in
      let base = { value; uid; depth } in
      resolve_tree ~on t oracle matrix ~base children
  | cursors ->
      let ( let* ) = Fiber.bind in
      let source = Carton.Value.source base.value in
      let source = bigstring_copy source in
      let rec go idx =
        if idx < Array.length cursors then begin
          let cursor = cursors.(idx) in
          let dirty = Carton.Value.source base.value in
          let src = Carton.Value.with_source ~source base.value in
          let value = Carton.of_offset_with_source t src ~cursor in
          let len = Carton.Value.length value
          and bstr = Carton.Value.bigstring value
          and kind = Carton.Value.kind value in
          let uid = identify oracle.Carton.identify ~kind ~len bstr
          and pos = oracle.Carton.where ~cursor
          and crc = oracle.Carton.checksum ~cursor
          and depth = succ base.depth in
          on value uid;
          matrix.(pos) <-
            Carton.Resolved_node
              { cursor; uid; crc; kind; depth; parent = base.uid };
          let children = oracle.Carton.children ~cursor ~uid in
          let children = Array.of_list children in
          let value = Carton.Value.with_source ~source:dirty value in
          let value = Carton.Value.flip value in
          let base = { value; uid; depth } in
          let* () = resolve_tree ~on t oracle matrix ~base children in
          go (succ idx)
        end
        else Fiber.return ()
      in
      go 0

let is_unresolved_base = function
  | Carton.Unresolved_base _ -> true
  | _ -> false

let verify ~on t oracle matrix =
  let ( let* ) = Fiber.bind in
  let mutex = Fiber.Mutex.create () in
  let idx = ref 0 in
  let rec f t =
    let* () = Fiber.Mutex.lock mutex in
    while
      !idx < Array.length matrix && is_unresolved_base matrix.(!idx) = false
    do
      incr idx
    done;
    let pos = !idx in
    incr idx;
    Fiber.Mutex.unlock mutex;
    if pos < Array.length matrix then begin
      let[@warning "-8"] (Carton.Unresolved_base { cursor }) = matrix.(pos) in
      let size = oracle.Carton.size ~cursor in
      let blob = Carton.Blob.make ~size in
      let value = Carton.of_offset t blob ~cursor in
      let len = Carton.Value.length value
      and bstr = Carton.Value.bigstring value
      and kind = Carton.Value.kind value in
      let uid = identify oracle.Carton.identify ~kind ~len bstr
      and crc = oracle.Carton.checksum ~cursor in
      on value uid;
      matrix.(pos) <- Carton.Resolved_base { cursor; uid; crc; kind };
      let children = oracle.Carton.children ~cursor ~uid in
      let children = Array.of_list children in
      let base = Carton.{ value = Value.flip value; uid; depth = 1 } in
      let* () = resolve_tree ~on t oracle matrix ~base children in
      f t
    end
    else Fiber.return ()
  in
  let init _thread = Carton.copy t in
  Fiber.parallel_iter ~f (List.init 4 init) >>= fun _units -> Fiber.return ()
