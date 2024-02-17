open Stdbob

let src = Logs.Src.create "bob.pack"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA1 = struct
  include Digestif.SHA1

  let length = digest_size
  let feed = feed_bigstring
  let null = digest_string ""
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
end

let rec full_write ~path fd bstr off len =
  let open Fiber in
  Fiber.write fd bstr ~off ~len >>= function
  | Ok len' when len' - len = 0 -> Fiber.return fd
  | Ok len' -> full_write ~path fd bstr (off + len') (len - len')
  | Error `Closed ->
      Fmt.failwith "Unexpected closed fd %d (%a)" (Obj.magic fd) Bob_fpath.pp
        path
  | Error (`Unix errno) ->
      Fmt.failwith "write(%d|%a): %s" (Obj.magic fd) Bob_fpath.pp path
        (Unix.error_message errno)

module First_pass = Carton.Dec.Fp (SHA1)
module Verify = Carton.Dec.Verify (SHA1) (Carton.Make (Fiber)) (Fiber)

type status = Verify.status
type decoder = First_pass.decoder

type entry =
  int64
  * status
  * [ `Base of [ `A | `B | `C | `D ] * Carton.Dec.weight
    | `Ofs of int * Carton.Dec.weight * Carton.Dec.weight * Carton.Dec.weight
    | `Ref of
      Digestif.SHA1.t
      * Carton.Dec.weight
      * Carton.Dec.weight
      * Carton.Dec.weight ]

let ctx = First_pass.ctx
let is_base = Verify.is_base
let is_resolved = Verify.is_resolved
let offset_of_status = Verify.offset_of_status
let uid_of_status = Verify.uid_of_status
let kind_of_status = Verify.kind_of_status
let pp_status = Verify.pp

let pp_kind ppf = function
  | `A -> Fmt.string ppf "A"
  | `B -> Fmt.string ppf "B"
  | `C -> Fmt.string ppf "C"
  | `D -> Fmt.string ppf "D"

let rec until_await_or_peek :
    reporter:(int -> unit Fiber.t) ->
    full:('acc -> bool Fiber.t) ->
    push:('acc -> 'a -> 'acc Fiber.t) ->
    acc:'acc ->
    Stdbob.bigstring ->
    First_pass.decoder ->
    (First_pass.decoder option * (Stdbob.bigstring * int) option * 'acc) Fiber.t
    =
 fun ~reporter ~full ~push ~acc src decoder ->
  let open Fiber in
  match First_pass.decode decoder with
  | `Await decoder -> Fiber.return (Some decoder, None, acc)
  | `Peek decoder ->
      let src_len = First_pass.src_rem decoder in
      if src_len > 0 then Fiber.return (Some decoder, Some (src, src_len), acc)
      else Fiber.return (Some decoder, None, acc)
  | `End hash ->
      Log.debug (fun m -> m "Hash of the PACK file: %a" SHA1.pp hash);
      push acc (`End hash, None, De.bigstring_empty, 0) >>= fun acc ->
      Fiber.return (None, None, acc)
  | `Malformed err -> failwith err
  | `Entry (entry, decoder) -> (
      match entry with
      | { First_pass.kind = Base kind; offset; size; _ } -> (
          Log.debug (fun m ->
              m "[%08Lx] Got a new entry (base, kind:%a, size:%d byte(s))."
                offset pp_kind kind
                (size :> int));
          let elt =
            `Elt
              (offset, Verify.unresolved_base ~cursor:offset, `Base (kind, size))
          in
          let off =
            let max = Bigarray.Array1.dim src in
            let len = First_pass.src_rem decoder in
            max - len
          in
          reporter 1 >>= fun () ->
          push acc (elt, Some decoder, src, off) >>= fun acc ->
          full acc >>= function
          | true when off = 0 -> Fiber.return (Some decoder, None, acc)
          | true -> Fiber.return (Some decoder, Some (src, off), acc)
          | false -> until_await_or_peek ~reporter ~full ~push ~acc src decoder)
      | { kind = Ofs { sub; source = s; target }; size; offset; _ } -> (
          let elt =
            `Elt (offset, Verify.unresolved_node, `Ofs (sub, s, target, size))
          in
          let off =
            let max = Bigarray.Array1.dim src in
            let len = First_pass.src_rem decoder in
            max - len
          in
          reporter 1 >>= fun () ->
          push acc (elt, Some decoder, src, off) >>= fun acc ->
          full acc >>= function
          | true when off = 0 -> Fiber.return (Some decoder, None, acc)
          | true -> Fiber.return (Some decoder, Some (src, off), acc)
          | false -> until_await_or_peek ~reporter ~full ~push ~acc src decoder)
      | { kind = Ref { ptr; source = s; target }; size; offset; _ } -> (
          let elt =
            `Elt (offset, Verify.unresolved_node, `Ref (ptr, s, target, size))
          in
          let off =
            let max = Bigarray.Array1.dim src in
            let len = First_pass.src_rem decoder in
            max - len
          in
          reporter 1 >>= fun () ->
          push acc (elt, Some decoder, src, off) >>= fun acc ->
          full acc >>= function
          | true when off = 0 -> Fiber.return (Some decoder, None, acc)
          | true -> Fiber.return (Some decoder, Some (src, off), acc)
          | false -> until_await_or_peek ~reporter ~full ~push ~acc src decoder)
      )

let analyse ?decoder reporter =
  let open Fiber in
  let flow (Stream.Sink k) =
    let init () =
      match decoder with
      | Some decoder ->
          k.init () >>= fun acc -> Fiber.return (Some decoder, None, acc)
      | None ->
          let oc = De.bigstring_create De.io_buffer_size in
          let zw = De.make_window ~bits:15 in
          let allocate _ = zw in
          let decoder = First_pass.decoder ~o:oc ~allocate `Manual in
          k.init () >>= fun acc -> Fiber.return (Some decoder, None, acc)
    in
    let rec push (decoder, previous, acc) next =
      k.full acc >>= function
      | true -> Fiber.return (decoder, previous, acc)
      | false -> (
          match (decoder, previous) with
          | None, _ -> Fiber.return (None, None, acc)
          | Some decoder, None ->
              let decoder =
                First_pass.src decoder next 0 (Bigarray.Array1.dim next)
              in
              until_await_or_peek ~reporter ~full:k.full ~push:k.push ~acc next
                decoder
          | Some decoder, Some (current, len) ->
              let max =
                min
                  (Bigarray.Array1.dim current - len)
                  (Bigarray.Array1.dim next)
              in
              if max > 0 then (
                bigstring_blit next ~src_off:0 current ~dst_off:len ~len:max;
                let decoder = First_pass.src decoder current 0 (len + max) in
                until_await_or_peek ~reporter ~full:k.full ~push:k.push ~acc
                  current decoder
                >>= fun ret ->
                push ret Bigarray.Array1.(sub next max (dim next - max)))
              else
                let tmp =
                  Bigarray.Array1.create Bigarray.char Bigarray.c_layout
                    (len + Bigarray.Array1.dim next)
                in
                bigstring_blit current ~src_off:0 tmp ~dst_off:0 ~len;
                bigstring_blit next ~src_off:0 tmp ~dst_off:len
                  ~len:(Bigarray.Array1.dim next);
                let decoder =
                  First_pass.src decoder tmp 0 (len + Bigarray.Array1.dim next)
                in
                until_await_or_peek ~reporter ~full:k.full ~push:k.push ~acc tmp
                  decoder)
    in
    let full (decoder, _, acc) =
      match decoder with None -> Fiber.return true | Some _ -> k.full acc
    in
    let stop (_, _, acc) = k.stop acc in
    Stream.Sink { init; stop; full; push }
  in
  { Stream.flow }

let inflate_entry ~reporter =
  let open Fiber in
  let flow (Stream.Sink k) =
    let init () = k.init () >>= fun acc -> Fiber.return (`Header, acc) in
    let rec push (state, acc) next =
      match state with
      | `Ignore -> Fiber.return (`Ignore, acc)
      | `Header ->
          Log.debug (fun m -> m "Consume the header entry into the PACK file.");
          let pos = ref 0 in
          let chr = ref (bigstring_get_uint8 next !pos) in
          while
            incr pos;
            !chr land 0x80 != 0 && !pos < Bigarray.Array1.dim next
          do
            chr := bigstring_get_uint8 next !pos
          done;
          if !chr land 0x80 == 0 then
            let allocate bits = De.make_window ~bits in
            let o = De.bigstring_create De.io_buffer_size in
            let decoder = Zl.Inf.decoder `Manual ~o ~allocate in
            if Bigarray.Array1.dim next - !pos = 0 then
              Fiber.return (`Inflate (o, Zl.Inf.decode decoder), acc)
            else
              push
                (`Inflate (o, Zl.Inf.decode decoder), acc)
                Bigarray.Array1.(sub next !pos (dim next - !pos))
          else Fiber.return (`Header, acc)
      | `Inflate (output, state) -> (
          match state with
          | `Flush decoder ->
              let len = Bigarray.Array1.dim output - Zl.Inf.dst_rem decoder in
              reporter len >>= fun () ->
              k.push acc (Bigarray.Array1.sub output 0 len) >>= fun acc ->
              push
                (`Inflate (output, Zl.Inf.(decode <.> flush) decoder), acc)
                next
          | `End decoder ->
              let len = Bigarray.Array1.dim output - Zl.Inf.dst_rem decoder in
              reporter len >>= fun () ->
              k.push acc (Bigarray.Array1.sub output 0 len) >>= fun acc ->
              Fiber.return (`Ignore, acc)
          | `Malformed err -> failwith err
          | `Await decoder ->
              let decoder =
                Zl.Inf.src decoder next 0 (Bigarray.Array1.dim next)
              in
              Fiber.return (`Inflate (output, Zl.Inf.decode decoder), acc))
    in
    let full (state, acc) =
      match state with `Ignore -> Fiber.return true | _ -> k.full acc
    in
    let stop (state, acc) =
      match state with
      | `Ignore | `Header -> k.stop acc
      | `Inflate (output, state) ->
          let rec go acc = function
            | `Await _ | `Malformed _ ->
                k.stop acc
                (* XXX(dinosaure): we probably need to signal [src decoder 0 0] for the
                   [`Await] case to ensure that we close the decoder but it seems to work. *)
            | `Flush decoder ->
                let len = Bigarray.Array1.dim output - Zl.Inf.dst_rem decoder in
                reporter len >>= fun () ->
                k.push acc (Bigarray.Array1.sub output 0 len) >>= fun acc ->
                go acc (Zl.Inf.(decode <.> flush) decoder)
            | `End decoder ->
                let len = Bigarray.Array1.dim output - Zl.Inf.dst_rem decoder in
                reporter len >>= fun () ->
                k.push acc (Bigarray.Array1.sub output 0 len) >>= k.stop
          in
          go acc state
    in
    Stream.Sink { init; stop; full; push }
  in
  { Stream.flow }

let replace tbl k v =
  match Hashtbl.find tbl k with
  | v' -> if v < v' then Hashtbl.replace tbl k v'
  | exception _ -> Hashtbl.add tbl k v

let make_window bits = De.make_window ~bits

let map (fd, st) ~pos len =
  let len = min (Int64.sub st.Unix.LargeFile.st_size pos) (Int64.of_int len) in
  let len = Int64.to_int len in
  let res =
    Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray res

let collect entries =
  let where = Hashtbl.create 0x100 in
  let child = Hashtbl.create 0x100 in
  let sized = Hashtbl.create 0x100 in
  let register (idx, acc) (offset, status, v) =
    match v with
    | `Base (_, weight) ->
        Hashtbl.add where offset idx;
        Hashtbl.add sized offset weight;
        Fiber.return (succ idx, status :: acc)
    | `Ofs (v, source, target, _weight) ->
        let v = Int64.(sub offset (of_int v)) in
        Log.debug (fun m -> m "An object references to another one at %08Lx" v);
        replace sized v source;
        replace sized offset target;
        Hashtbl.add where offset idx;
        (try
           let vs = Hashtbl.find child (`Ofs v) in
           Hashtbl.replace child (`Ofs v) (offset :: vs)
         with _ -> Hashtbl.add child (`Ofs v) [ offset ]);
        Fiber.return (succ idx, status :: acc)
    | `Ref (ptr, source, target, _weight) ->
        Log.debug (fun m ->
            m "An object references to another one: %a" SHA1.pp ptr);
        replace sized offset (max target source);
        Hashtbl.add where offset idx;
        (try
           let vs = Hashtbl.find child (`Ref ptr) in
           Hashtbl.replace child (`Ref ptr) (offset :: vs)
         with _ -> Hashtbl.add child (`Ref ptr) [ offset ]);
        Fiber.return (succ idx, status :: acc)
  in
  let open Fiber in
  Stream.Source.fold register (0, []) entries >>= fun (_max, entries) ->
  let matrix = Array.of_list (List.rev entries) in
  let where ~cursor = Hashtbl.find where cursor in
  let children ~cursor ~uid =
    match
      (Hashtbl.find_opt child (`Ofs cursor), Hashtbl.find_opt child (`Ref uid))
    with
    | Some a, Some b -> List.sort_uniq compare (a @ b)
    | Some x, None | None, Some x -> x
    | None, None -> []
  in
  let weight ~cursor = Hashtbl.find sized cursor in
  let oracle = { Carton.Dec.where; children; digest = Git.digest; weight } in
  Fiber.return (matrix, oracle)

let verify ?reporter:(verbose = ignore) ~oracle path matrix =
  let open Fiber in
  let fd = Unix.openfile (Bob_fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
  let st = Unix.LargeFile.stat (Bob_fpath.to_string path) in
  let pack =
    Carton.Dec.make (fd, st) ~allocate:make_window
      ~z:(De.bigstring_create De.io_buffer_size)
      ~uid_ln:SHA1.length ~uid_rw:SHA1.of_raw_string Stdbob.never
  in
  Verify.verify ~threads:4 pack ~map ~oracle ~verbose ~matrix >>= fun () ->
  Unix.close fd;
  Fiber.return ()

let rec create_filesystem ~reporter pack =
  let init = Fiber.always pack in
  let push pack = function
    | `Dir (path, hash) ->
        reporter 1;
        create_directory ~reporter pack path hash
    | `Reg (path, hash) ->
        reporter 1;
        create_file pack path hash
  in
  let full _ = Fiber.return false in
  let stop _ = Fiber.return () in
  Stream.Sink { init; push; full; stop }

and create_directory ~reporter pack path hash =
  if not (Sys.file_exists (Bob_fpath.to_string path)) then
    Sys.mkdir (Bob_fpath.to_string path) 0o755;
  let contents =
    Carton.Dec.weight_of_uid ~map pack ~weight:Carton.Dec.null hash
    |> fun weight ->
    let raw = Carton.Dec.make_raw ~weight in
    Carton.Dec.of_uid ~map pack raw hash
  in
  let open Fiber in
  let contents =
    Bigarray.Array1.sub (Carton.Dec.raw contents) 0 (Carton.Dec.len contents)
  in
  Stream.Stream.run
    ~from:(Git.tree_of_cstruct ~path contents)
    ~via:Stream.Flow.identity
    ~into:(create_filesystem ~reporter pack)
  >>= function
  | (), None -> Fiber.return pack
  | (), Some _ -> failwith "Tree entry partially consumed"

and create_file pack path hash =
  let contents =
    Carton.Dec.weight_of_uid ~map pack ~weight:Carton.Dec.null hash
    |> fun weight ->
    let raw = Carton.Dec.make_raw ~weight in
    Carton.Dec.of_uid ~map pack raw hash
  in
  let open Fiber in
  let bstr =
    Bigarray.Array1.sub (Carton.Dec.raw contents) 0 (Carton.Dec.len contents)
  in
  Fiber.openfile path Unix.[ O_CREAT; O_TRUNC; O_WRONLY; O_APPEND ] 0o644
  >>= function
  | Error errno ->
      Log.err (fun m ->
          m "Impossible to save %a: %s" Bob_fpath.pp path
            (Unix.error_message errno));
      Fiber.return pack
  | Ok fd ->
      let open Fiber in
      Fiber.catch
        (fun () ->
          full_write ~path fd bstr 0 (Bigarray.Array1.dim bstr) >>= Fiber.close)
        (fun exn -> Fiber.close fd >>= fun () -> raise exn)
      >>= fun () -> Fiber.return pack

let pack path status =
  let fd = Unix.openfile (Bob_fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
  let st = Unix.LargeFile.stat (Bob_fpath.to_string path) in
  let id =
    Array.fold_left
      (fun tbl status ->
        Hashtbl.add tbl
          (Verify.uid_of_status status)
          (Verify.offset_of_status status);
        tbl)
      (Hashtbl.create 0x100) status
  in
  let find uid =
    Logs.debug (fun m -> m "Try to find: %a." SHA1.pp uid);
    Hashtbl.find id uid
  in
  let pack =
    Carton.Dec.make (fd, st) ~allocate:make_window
      ~z:(De.bigstring_create De.io_buffer_size)
      ~uid_ln:SHA1.length ~uid_rw:SHA1.of_raw_string find
  in
  Fiber.return pack
