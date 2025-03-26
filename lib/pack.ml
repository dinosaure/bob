open Stdbob

let src = Logs.Src.create "bob.pack"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA1 = struct
  include Digestif.SHA1

  let length = digest_size
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
end

type store = {
  store : (SHA1.t, Bob_fpath.t) Hashtbl.t;
  rstore : (Bob_fpath.t, SHA1.t * [ `Dir | `Reg | `Root ]) Hashtbl.t;
  root : (SHA1.t * SHA1.t) option;
  path : Bob_fpath.t;
}

let length { rstore; _ } =
  let module Set = Set.Make (SHA1) in
  let hashes =
    Hashtbl.fold (fun _ (hash, _) set -> Set.add hash set) rstore Set.empty
  in
  Set.cardinal hashes

module Filesystem = struct
  let readdir =
    let readdir d =
      try Sys.readdir (Bob_fpath.to_string d) with _exn -> [||]
    in
    Array.to_list <.> readdir

  let rec traverse ~get ~add visited stack ~f acc =
    match stack with
    | [] -> Fiber.return acc
    | x :: r ->
        if List.exists (Bob_fpath.equal x) visited then
          traverse ~get ~add visited r ~f acc
        else
          let open Fiber in
          let contents = get x in
          traverse ~get ~add (x :: visited) (add contents stack) ~f acc >>= f x

  let fold ?(dotfiles = false) ~f acc paths =
    let dir_child d acc bname =
      if (not dotfiles) && bname.[0] = '.' then acc
      else Bob_fpath.(d / bname) :: acc
    in
    let add stack vs = vs @ stack in
    let get path =
      let entries = readdir path in
      List.fold_left (dir_child path) [] entries
    in
    traverse ~get ~add [] paths ~f acc

  let fold ?dotfiles ~f acc d = fold ?dotfiles ~f acc [ d ]
end

external bigstring_read :
  Unix.file_descr -> Stdbob.bigstring -> int -> int -> int
  = "bob_bigstring_read"
[@@noalloc]

let rec full_read fd ba off len =
  if len > 0 then
    match bigstring_read fd ba off len with
    | ret when ret < 0 ->
        Fmt.failwith "Got an error while reading %d" (Obj.magic fd)
    | len' when len - len' > 0 -> full_read fd ba (off + len') (len - len')
    | _ -> ()

(* XXX(dinosaure): [mmap] can be a solution here but it seems that the kernel
   does many page-faults if the usage is load many files. [mmap] becomes
   interesting when we load several times the same file at different offsets
   (cc XXX(rand)). We will stay simple and see how we can improve that. *)
let load_file stat path =
  let open Fiber in
  Log.debug (fun m -> m "Load the file: %a." Bob_fpath.pp path);
  let len = stat.Unix.st_size in
  Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
  | Ok fd ->
      let res = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
      let finally () = Fiber.close fd in
      Fiber.protect ~finally @@ fun () ->
      full_read fd res 0 len;
      let value = Carton.Value.make ~kind:`C res in
      Fiber.return value
  | Error errno ->
      Fmt.failwith "openfile(%a): %s" Bob_fpath.pp path
        (Unix.error_message errno)

let load_directory rstore path =
  Log.debug (fun m -> m "Load directory: %a." Bob_fpath.pp path);
  let entries = Filesystem.readdir path in
  let entries =
    List.filter_map
      (fun entry ->
        let key = Bob_fpath.(path / entry) in
        Log.debug (fun m -> m "Try to find: %a." Bob_fpath.pp key);
        match Hashtbl.find_opt rstore key with
        | Some (hash, `Dir) -> Some (Bob_fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Bob_fpath.v entry, hash)
        | Some (_, `Root) -> None
        | None -> None)
      entries
  in
  let open Fiber in
  let open Stream in
  let lst = Git.serialize_directory entries in
  let stream = Stream.of_list lst in
  Stream.to_string stream >>| fun str -> Carton.Value.of_string ~kind:`B str

let load_root ~real_length path hash =
  let basename = Bob_fpath.basename path in
  let root = SHA1.to_raw_string hash in
  let entry = Fmt.str "%s\000%s%d" basename root real_length in
  Fiber.return (Carton.Value.of_string ~kind:`A entry)

let load : store -> Carton.Uid.t -> Carton.Value.t Fiber.t =
 fun store uid ->
  let hash = SHA1.of_raw_string (uid :> string) in
  match (Hashtbl.find_opt store.store hash, store.root) with
  | None, Some (hash_of_root, hash_of_tree) when hash_of_root = hash ->
      Log.debug (fun m -> m "Load root object.");
      let real_length = Hashtbl.length store.rstore in
      load_root ~real_length store.path hash_of_tree
  | Some path, _ -> (
      let real_path = Bob_fpath.(normalize (store.path // path)) in
      let stat = Unix.stat (Bob_fpath.to_string real_path) in
      Log.debug (fun m ->
          m "Load %a object (%a)." Bob_fpath.pp real_path Bob_fpath.pp path);
      match stat.Unix.st_kind with
      | Unix.S_REG -> load_file stat real_path
      | Unix.S_DIR -> load_directory store.rstore real_path
      | _ -> failwith "Invalid kind of object")
  | None, (Some _ | None) ->
      Log.err (fun m -> m "The object %a does not exists." SHA1.pp hash);
      raise Not_found

let entry_of_file stat _real_path uid =
  Cartonnage.Entry.make ~kind:`B ~length:stat.Unix.st_size uid ()
  |> Fiber.return

let entry_of_directory _stat rstore path uid =
  let entries = Filesystem.readdir path in
  let entries =
    List.filter_map
      (fun entry ->
        let key = Bob_fpath.(path / entry) in
        match Hashtbl.find_opt rstore key with
        | Some (hash, `Dir) -> Some (Bob_fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Bob_fpath.v entry, hash)
        | Some (_, `Root) -> None
        | None -> None)
      entries
  in
  let lst = Git.serialize_directory entries in
  let sum acc str = acc + String.length str in
  let length = List.fold_left sum 0 lst in
  Fiber.return (Cartonnage.Entry.make ~kind:`C ~length uid ())

let entry : store -> Carton.Uid.t -> unit Cartonnage.Entry.t Fiber.t =
 fun store uid ->
  let hash = Digestif.SHA1.of_raw_string (uid :> string) in
  match (Hashtbl.find_opt store.store hash, store.root) with
  | None, Some (hash_of_root, hash_of_tree) when hash_of_root = hash ->
      let basename = Bob_fpath.basename store.path in
      let root = SHA1.to_raw_string hash_of_tree in
      let real_length = Hashtbl.length store.rstore in
      let str = Fmt.str "%s\000%s%d" basename root real_length in
      Cartonnage.Entry.make ~kind:`A ~length:(String.length str) uid ()
      |> Fiber.return
  | Some path, _ -> (
      let real_path = Bob_fpath.(normalize (store.path // path)) in
      let stat = Unix.stat (Bob_fpath.to_string real_path) in
      match stat.Unix.st_kind with
      | Unix.S_REG -> entry_of_file stat real_path uid
      | Unix.S_DIR -> entry_of_directory stat store.rstore real_path uid
      | _ -> failwith "Invalid kind of object")
  | None, (Some _ | None) -> raise Not_found

let deltify ~reporter ?(compression = true) store hashes =
  let open Fiber in
  match compression with
  | true ->
      let fn = entry store in
      let entries = Stream.Stream.map fn hashes in
      let load uid () = load store uid in
      Bob_carton.delta ~reporter ~load entries
      (*
      (* XXX(dinosaure): do the delta compression. *)
      let module Verbose = struct
        type +'a fiber = 'a Fiber.t

        let counter = ref 0

        let succ () =
          incr counter;
          Fiber.return ()

        let print () =
          let open Fiber in
          reporter !counter >>| fun () -> counter := 0
      end in
      let module Delta = Carton.Enc.Delta (Scheduler) (Fiber) (SHA1) (Verbose)
      in
      let open Fiber in
      let f hash =
        load store hash |> Scheduler.prj >>= fun v ->
        let kind = Carton.Dec.kind v in
        let length = Carton.Dec.len v in
        let entry = Carton.Enc.make_entry ~kind ~length hash in
        Fiber.return entry
      in
      let entries = Stream.Stream.map f hashes in
      Stream.Stream.to_array entries >>= fun entries ->
      Delta.delta
        ~threads:(List.init 4 (fun _ -> load store))
        ~weight:10 ~uid_ln:SHA1.length entries
      >>| Stream.Stream.of_array
      *)
  | false ->
      (* XXX(dinosaure): just generate targets without patch compression. *)
      let fn uid = entry store uid >>| Cartonnage.Target.make in
      Stream.Stream.map fn hashes

(*
      let uid =
        {
          Carton.Enc.uid_ln = SHA1.digest_size;
          Carton.Enc.uid_rw = SHA1.to_raw_string;
        }
      in
      let b =
        {
          Carton.Enc.o =
            Bigarray.Array1.create Bigarray.char Bigarray.c_layout len;
          Carton.Enc.i =
            Bigarray.Array1.create Bigarray.char Bigarray.c_layout
              io_buffer_size;
          Carton.Enc.q = De.Queue.create 0x1000;
          Carton.Enc.w = De.Lz77.make_window ~bits:15;
        }
      in

      let open Fiber in
      k.init () >>= fun acc ->
      let hdr = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 12 in
      let ctx = SHA1.empty in
      Carton.Enc.header_of_pack ~length hdr 0 12;
      let ctx = SHA1.feed_bigstring ctx hdr ~off:0 ~len:12 in
      k.push acc hdr >>= fun acc ->
      Fiber.return
        ({ ctx; cursor = 12L; b; uid; offsets = Hashtbl.create length }, acc)
    in
    let push (t, acc) target =
      let open Fiber in
      reporter () >>= fun () ->
      let anchor = t.cursor in
      Hashtbl.add t.offsets (Carton.Enc.target_uid target) t.cursor;
      let find hash =
        match Hashtbl.find_opt t.offsets hash with
        | Some v ->
            Log.debug (fun m ->
                m "Ask where is %a: %08Lx (anchor: %08Lx)." SHA1.pp hash v
                  anchor);
            Fiber.return (Some (Int64.to_int v))
        | None ->
            Log.err (fun m -> m "%a not found." SHA1.pp hash);
            Fiber.return None
      in

      Log.debug (fun m ->
          m "Start to encode %a at %08Lx" SHA1.pp
            (Carton.Enc.target_uid target)
            t.cursor);
      Carton.Enc.encode_target ?level scheduler ~b:t.b
        ~find:(Scheduler.inj <.> find) ~load:(load store) ~uid:t.uid target
        ~cursor:(Int64.to_int t.cursor)
      |> Scheduler.prj
      >>= fun (len, encoder) ->
      k.push acc (Bigarray.Array1.sub t.b.o 0 len) >>= fun acc ->
      t.ctx <- SHA1.feed_bigstring t.ctx t.b.o ~off:0 ~len;
      t.cursor <- Int64.add t.cursor (Int64.of_int len);
      let encoder =
        Carton.Enc.N.dst encoder t.b.o 0 (Bigarray.Array1.dim t.b.o)
      in
      encode_target t ~push:k.push ~acc encoder >>= fun acc ->
      Fiber.return (t, acc)
    in
    let full (_, acc) = k.full acc in
    let stop (t, acc) =
      let open Fiber in
      let hash = SHA1.get t.ctx in
      Log.debug (fun m -> m "Hash of the PACK file: %a" SHA1.pp hash);
      let hash = SHA1.to_raw_string hash in
      let hash = bigstring_of_string hash ~off:0 ~len:(String.length hash) in
      k.push acc hash >>= k.stop
    in
    Stream.Sink { init; stop; full; push }
  in
  { Stream.flow }
*)

let store root =
  let open Fiber in
  let rstore : (Bob_fpath.t, SHA1.t * [ `Reg | `Dir | `Root ]) Hashtbl.t =
    Hashtbl.create 0x100
  in
  let compute path =
    Log.debug (fun m -> m "Compute %a." Bob_fpath.pp path);
    let stat = Unix.stat (Bob_fpath.to_string path) in
    match stat.Unix.st_kind with
    | Unix.S_REG ->
        Git.hash_of_filename path >>= fun hash ->
        Log.debug (fun m -> m "%a -> %a" Bob_fpath.pp path SHA1.pp hash);
        Hashtbl.add rstore path (hash, `Reg);
        Fiber.return (Some hash)
    | Unix.S_DIR ->
        Log.debug (fun m -> m "Calculate the hash of %a" Bob_fpath.pp path);
        Git.hash_of_directory ~root rstore path >>= fun hash ->
        Log.debug (fun m -> m "%a -> %a" Bob_fpath.pp path SHA1.pp hash);
        Hashtbl.add rstore path (hash, `Dir);
        Fiber.return (Some hash)
    | _ -> Fiber.return None
  in
  let open Stream in
  let paths =
    Stream.of_iter @@ fun f ->
    Filesystem.fold ~dotfiles:false ~f:(fun path () -> f path) () root
  in
  let hashes = Stream.filter_map compute paths in
  (* XXX(dinosaure): populate [rstore] and [store]. *)
  Stream.to_list hashes >>= fun hashes ->
  let store = Hashtbl.create (Hashtbl.length rstore) in
  Hashtbl.iter
    (fun v (k, _) ->
      let v = Stdlib.Option.get (Bob_fpath.relativize ~root v) in
      Hashtbl.add store k v)
    rstore;
  let hashes_for_directory =
    if Sys.is_directory (Bob_fpath.to_string root) then (
      let hash_of_tree, _ = Hashtbl.find rstore root in
      let real_length = Hashtbl.length rstore in
      let hash_of_root = Git.hash_of_root ~real_length ~root hash_of_tree in
      Log.debug (fun m -> m "Hash of root: %a" SHA1.pp hash_of_root);
      Log.debug (fun m ->
          m "Hash of tree: %a (%a)" SHA1.pp hash_of_tree Bob_fpath.pp root);
      Hashtbl.add rstore (Bob_fpath.v "./") (hash_of_root, `Root);
      Some (hash_of_root, hash_of_tree))
    else None
  in
  let hashes =
    match hashes_for_directory with
    | Some (hash_of_root, _) -> hash_of_root :: hashes
    | None -> hashes
  in
  let module Set = Set.Make (SHA1) in
  let hashes = List.fold_left (rev Set.add) Set.empty hashes in
  let hashes = Set.elements hashes in
  let hash_to_uid = Carton.Uid.unsafe_of_string <.> SHA1.to_raw_string in
  let uids = List.map hash_to_uid hashes in
  Log.debug (fun m -> m "Store of %a." Bob_fpath.pp root);
  let stream = Stream.of_list uids in
  let store = { store; rstore; root = hashes_for_directory; path = root } in
  Fiber.return (stream, store)

let make ?level ~reporter store =
  let length = length store in
  let load uid () = load store uid in
  Bob_carton.pack ~reporter ?level ~length load

let _C = 0b011
let _D = 0b100

let encode_header_of_entry ~kind ~length =
  let buf = Buffer.create 16 in
  let c = ref ((kind lsl 4) lor (length land 15)) in
  let l = ref (length asr 4) in
  while !l != 0 do
    Buffer.add_char buf (Char.chr (!c lor 0x80 land 0xff));
    c := !l land 0x7f;
    l := !l asr 7
  done;
  Buffer.add_char buf (Char.chr !c);
  Buffer.contents buf

let entry_with_filename ?level path =
  let rec deflate zl buf output =
    match Zl.Def.encode zl with
    | `Await zl -> deflate (Zl.Def.src zl De.bigstring_empty 0 0) buf output
    | `Flush zl ->
        let len = De.bigstring_length output - Zl.Def.dst_rem zl in
        let str = Stdbob.bigstring_substring ~off:0 ~len output in
        Buffer.add_string buf str;
        deflate (Zl.Def.dst zl output 0 (De.bigstring_length output)) buf output
    | `End zl ->
        let len = De.bigstring_length output - Zl.Def.dst_rem zl in
        let str = Stdbob.bigstring_substring ~off:0 ~len output in
        Buffer.add_string buf str;
        Buffer.contents buf
  in
  let entry = Bob_fpath.basename path in
  let deflated =
    let q = De.Queue.create 0x1000 in
    let w = De.Lz77.make_window ~bits:15 in
    let o = De.bigstring_create 0x100 in
    let level = Option.value ~default:4 level in
    let z = Zl.Def.encoder ~q ~w ~level `Manual `Manual in
    let i =
      let len = String.length entry in
      Stdbob.bigstring_of_string entry ~off:0 ~len
    in
    let z = Zl.Def.src z i 0 (String.length entry) in
    let z = Zl.Def.dst z o 0 (De.bigstring_length o) in
    deflate z (Buffer.create 0x100) o
  in
  let deflated =
    let len = String.length deflated in
    Stdbob.bigstring_of_string deflated ~off:0 ~len
  in
  let hdr_entry =
    let length = String.length entry in
    encode_header_of_entry ~kind:_D ~length
  in
  let hdr_entry =
    let len = String.length hdr_entry in
    bigstring_of_string hdr_entry ~off:0 ~len
  in
  (hdr_entry, deflated)

let make_one ?(level = 4) ~reporter ~finalise path =
  let open Fiber in
  let len = 0x7ff in
  let bstr = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
  let fn str =
    let len = String.length str in
    bigstring_blit_from_string str ~src_off:0 bstr ~dst_off:0 ~len;
    Bigarray.Array1.sub bstr 0 len
  in
  Stream.Stream.of_file ~len ~fn path >>= function
  | Error (`Msg msg) as err ->
      Log.err (fun m ->
          m "The file %a does not exists: %s" Bob_fpath.pp path msg);
      Fiber.return err
  | Ok file ->
      let hdr = Fmt.str "PACK\000\000\000\002\000\000\000\002" in
      let hdr = hdr in
      let hdr = bigstring_of_string hdr ~off:0 ~len:(String.length hdr) in
      let ctx = ref (SHA1.feed_bigstring SHA1.empty hdr) in
      let q = De.Queue.create 0x1000 in
      let w = De.Lz77.make_window ~bits:15 in
      let open Stream in
      let hdr_name, name = entry_with_filename ~level path in
      let hdr_file =
        encode_header_of_entry ~kind:_C
          ~length:(Unix.stat (Bob_fpath.to_string path)).Unix.st_size
        |> fun str -> bigstring_of_string str ~off:0 ~len:(String.length str)
      in
      let zlib = Flow.deflate_zlib ~len ~q ~w level in
      let file = Stream.tap (reporter <.> Bigarray.Array1.dim) file in
      let file = Stream.via zlib file in
      let name =
        Stream.tap
          (fun bstr ->
            Log.debug (fun m -> m "Calculate the PACK hash with:");
            Log.debug (fun m ->
                m "@[<hov>%a@]"
                  (Hxd_string.pp Hxd.default)
                  (bigstring_to_string bstr));
            ctx := SHA1.feed_bigstring !ctx bstr;
            Fiber.return ())
          (Stream.double hdr_name name)
      in
      let file =
        Stream.tap
          (fun bstr ->
            Log.debug (fun m -> m "Calculate the PACK hash with:");
            Log.debug (fun m ->
                m "@[<hov>%a@]"
                  (Hxd_string.pp Hxd.default)
                  (bigstring_to_string bstr));
            ctx := SHA1.feed_bigstring !ctx bstr;
            Fiber.return ())
          Stream.(singleton hdr_file ++ file)
      in
      let open Stream in
      let fn () =
        finalise ();
        let hash = SHA1.get !ctx in
        Log.debug (fun m -> m "Hash of the PACK file: %a." SHA1.pp hash);
        let len = SHA1.length and off = 0 in
        let res = (bigstring_of_string ~off ~len <.> SHA1.to_raw_string) hash in
        Fiber.return res
      in
      singleton hdr ++ name ++ file ++ of_fiber fn |> fun stream ->
      Log.debug (fun m -> m "The PACK stream is ready to be sent.");
      Fiber.return (Ok stream)

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
    let full = function
      | `Ignore, _ -> Fiber.return true
      | _, acc -> k.full acc
    in
    let stop (state, acc) =
      match state with
      | `Ignore | `Header -> k.stop acc
      | `Inflate (output, state) ->
          let rec go acc = function
            | `Await _ | `Malformed _ ->
                k.stop acc
                (* XXX(dinosaure): we probably need to signal [src decoder 0 0]
                   for the [`Await] case to ensure that we close the decoder but
                   it seems to work. *)
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

let rec until_await_or_peek :
    reporter:(int -> unit Fiber.t) ->
    full:('acc -> bool Fiber.t) ->
    push:('acc -> 'a -> 'acc Fiber.t) ->
    acc:'acc ->
    Stdbob.bigstring ->
    Carton.First_pass.decoder ->
    (Carton.First_pass.decoder option * (Stdbob.bigstring * int) option * 'acc)
    Fiber.t =
 fun ~reporter ~full ~push ~acc src decoder ->
  let open Carton in
  let open Fiber in
  let ( let* ) = Fiber.bind in
  match First_pass.decode decoder with
  | `Await decoder -> Fiber.return (Some decoder, None, acc)
  | `Peek decoder ->
      let src_len = Carton.First_pass.src_rem decoder in
      if src_len > 0 then Fiber.return (Some decoder, Some (src, src_len), acc)
      else Fiber.return (Some decoder, None, acc)
  | `End hash ->
      Log.debug (fun m -> m "Hash of the PACK file: %s" (Ohex.encode hash));
      push acc (`End hash, None, De.bigstring_empty, 0) >>= fun acc ->
      Fiber.return (None, None, acc)
  | `Malformed err -> failwith err
  | `Entry (entry, decoder) -> (
      match entry with
      | { First_pass.kind = Base (kind, _); offset; size; _ } ->
          Log.debug (fun m ->
              m "[%08x] Got a new entry (base, kind:%a, size:%d byte(s))."
                offset Carton.Kind.pp kind
                (size :> int));
          let status = Carton.Unresolved_base { cursor = offset } in
          let elt = `Elt (offset, status, `Base (kind, size)) in
          let off =
            let max = Bigarray.Array1.dim src in
            let len = First_pass.src_rem decoder in
            max - len
          in
          reporter 1 >>= fun () ->
          push acc (elt, Some decoder, src, off) >>= fun acc ->
          let* is_full = full acc in
          if is_full && off = 0 then Fiber.return (Some decoder, None, acc)
          else if is_full then Fiber.return (Some decoder, Some (src, off), acc)
          else until_await_or_peek ~reporter ~full ~push ~acc src decoder
      | { kind = Ofs { sub; source = s; target }; size; offset; _ } ->
          let status = Carton.Unresolved_node in
          let elt = `Elt (offset, status, `Ofs (sub, s, target, size)) in
          let off =
            let max = Bigarray.Array1.dim src in
            let len = First_pass.src_rem decoder in
            max - len
          in
          reporter 1 >>= fun () ->
          push acc (elt, Some decoder, src, off) >>= fun acc ->
          let* is_full = full acc in
          if is_full && off = 0 then Fiber.return (Some decoder, None, acc)
          else if is_full then Fiber.return (Some decoder, Some (src, off), acc)
          else until_await_or_peek ~reporter ~full ~push ~acc src decoder
      | { kind = Ref { ptr; source = s; target }; size; offset; _ } ->
          let status = Carton.Unresolved_node in
          let elt = `Elt (offset, status, `Ref (ptr, s, target, size)) in
          let off =
            let max = Bigarray.Array1.dim src in
            let len = First_pass.src_rem decoder in
            max - len
          in
          reporter 1 >>= fun () ->
          push acc (elt, Some decoder, src, off) >>= fun acc ->
          let* is_full = full acc in
          if is_full && off = 0 then Fiber.return (Some decoder, None, acc)
          else if is_full then Fiber.return (Some decoder, Some (src, off), acc)
          else until_await_or_peek ~reporter ~full ~push ~acc src decoder)

type status = Carton.status
type decoder = Carton.First_pass.decoder

type entry =
  int
  * status
  * [ `Base of Carton.Kind.t * Carton.Size.t
    | `Ofs of int * Carton.Size.t * Carton.Size.t * Carton.Size.t
    | `Ref of Carton.Uid.t * Carton.Size.t * Carton.Size.t * Carton.Size.t ]

type elt = [ `End of string | `Elt of entry ]

let analyse ?decoder reporter =
  let open Fiber in
  let open Carton in
  let ( let* ) = Fiber.bind in
  let flow (Stream.Sink k) =
    let init () =
      match decoder with
      | Some decoder ->
          let* acc = k.init () in
          Fiber.return (Some decoder, None, acc)
      | None ->
          let output = De.bigstring_create De.io_buffer_size in
          let zw = De.make_window ~bits:15 in
          let allocate _ = zw in
          let ref_length = SHA1.length in
          let digest = Git.digest in
          let identify = Git.identify in
          let decoder =
            First_pass.decoder ~output ~allocate ~ref_length ~digest ~identify
              `Manual
          in
          let* acc = k.init () in
          Fiber.return (Some decoder, None, acc)
    in
    let rec push (decoder, previous, acc) next =
      let* is_full = k.full acc in
      if is_full then Fiber.return (decoder, previous, acc)
      else
        match (decoder, previous) with
        | None, _ -> Fiber.return (None, None, acc)
        | Some decoder, None ->
            let len = Bigarray.Array1.dim next in
            Log.debug (fun m -> m "Analyze PACK file:");
            Log.debug (fun m ->
                m "@[<hov>%a@]"
                  (Hxd_string.pp Hxd.default)
                  (Stdbob.bigstring_to_string next));
            let decoder = First_pass.src decoder next 0 len in
            until_await_or_peek ~reporter ~full:k.full ~push:k.push ~acc next
              decoder
        | Some decoder, Some (current, len) ->
            let max =
              Int.min
                (Bigarray.Array1.dim current - len)
                (Bigarray.Array1.dim next)
            in
            if max > 0 then begin
              bigstring_blit next ~src_off:0 current ~dst_off:len ~len:max;
              let decoder = First_pass.src decoder current 0 (len + max) in
              until_await_or_peek ~reporter ~full:k.full ~push:k.push ~acc
                current decoder
              >>= fun ret ->
              push ret Bigarray.Array1.(sub next max (dim next - max))
            end
            else begin
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
                decoder
            end
    in
    let full = function
      | None, _, _ -> Fiber.return true
      | _, _, acc -> k.full acc
    in
    let stop (_, _, acc) = k.stop acc in
    Stream.Sink { init; stop; full; push }
  in
  { Stream.flow }

let full_write ~path fd bstr off len =
  let buf = Bytes.create 0x7ff in
  let ( let* ) = Fiber.bind in
  let rec go (src_off, rem) =
    if len = 0 then Fiber.return fd
    else begin
      let len = Int.min (Bytes.length buf) rem in
      bigstring_blit_to_bytes bstr ~src_off buf ~dst_off:0 ~len;
      let* did = Fiber.write fd (Bytes.unsafe_to_string buf) ~off:0 ~len in
      match did with
      | Ok len' -> go (src_off + len', rem - len')
      | Error `Closed ->
          Fmt.failwith "Unexpected closed fd %d (%a)" (Obj.magic fd)
            Bob_fpath.pp path
      | Error (`Unix errno) ->
          Fmt.failwith "write(%d|%a): %s" (Obj.magic fd) Bob_fpath.pp path
            (Unix.error_message errno)
    end
  in
  go (off, len)

let rec create_filesystem ~reporter pack =
  let init = Fiber.always pack in
  let push pack = function
    | `Dir (path, uid) ->
        reporter 1;
        create_directory ~reporter pack path uid
    | `Reg (path, uid) ->
        reporter 1;
        create_file pack path uid
  in
  let full _ = Fiber.return false in
  let stop _ = Fiber.return () in
  Stream.Sink { init; push; full; stop }

and create_directory ~reporter pack path uid =
  if not (Sys.file_exists (Bob_fpath.to_string path)) then
    Sys.mkdir (Bob_fpath.to_string path) 0o755;
  let contents =
    let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
    let blob = Carton.Blob.make ~size in
    Carton.of_uid pack blob ~uid
  in
  let open Fiber in
  let off = 0 and len = Carton.Value.length contents in
  let contents = Carton.Value.bigstring contents in
  let contents = Bigarray.Array1.sub contents off len in
  Stream.Stream.run
    ~from:(Git.tree_of_bstr ~path contents)
    ~via:Stream.Flow.identity
    ~into:(create_filesystem ~reporter pack)
  >>= function
  | (), None -> Fiber.return pack
  | (), Some _ -> failwith "Tree entry partially consumed"

and create_file pack path uid =
  let contents =
    let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
    let blob = Carton.Blob.make ~size in
    Carton.of_uid pack blob ~uid
  in
  let open Fiber in
  let off = 0 and len = Carton.Value.length contents in
  let bstr = Carton.Value.bigstring contents in
  let bstr = Bigarray.Array1.sub bstr off len in
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

let update_size sized ~parent offset size =
  let cell : Carton.Size.t ref = Hashtbl.find sized parent in
  (cell := Carton.Size.(max !cell size));
  Hashtbl.add sized offset cell

let new_child (boff, bptr) ~parent child =
  match parent with
  | `Ofs parent -> begin
      match Hashtbl.find_opt boff parent with
      | None -> Hashtbl.add boff parent [ child ]
      | Some offs -> Hashtbl.replace boff parent (child :: offs)
    end
  | `Ref parent -> begin
      match Hashtbl.find_opt bptr parent with
      | None -> Hashtbl.add bptr parent [ child ]
      | Some offs -> Hashtbl.add bptr parent (child :: offs)
    end

let collect s =
  let where = Hashtbl.create 0x100 in
  let child = (Hashtbl.create 0x100, Hashtbl.create 0x100) in
  let sized = Hashtbl.create 0x100 in
  let based = Hashtbl.create 0x100 in
  let register (idx, acc) (offset, status, v) =
    match v with
    | `Base (_, size) ->
        Hashtbl.add where offset idx;
        Hashtbl.add sized offset (ref size);
        Hashtbl.add based idx offset;
        Fiber.return (succ idx, status :: acc)
    | `Ofs (sub, source, target, _size) ->
        let parent = offset - sub in
        update_size sized ~parent offset (Carton.Size.max source target);
        new_child child ~parent:(`Ofs parent) offset;
        Hashtbl.add where offset idx;
        Fiber.return (succ idx, status :: acc)
    | `Ref (ptr, source, target, _size) ->
        let size = Carton.Size.max source target in
        (* NOTE(dinosaure): we don't generate [Ref] objects. *)
        Hashtbl.add sized offset (ref size);
        new_child child ~parent:(`Ref ptr) offset;
        Hashtbl.add where offset idx;
        Fiber.return (succ idx, status :: acc)
  in
  let ( let* ) = Fiber.bind in
  let* number_of_objects, entries = Stream.Source.fold register (0, []) s in
  let matrix = Array.of_list (List.rev entries) in
  let where ~cursor = Hashtbl.find where cursor in
  let size ~cursor = !(Hashtbl.find sized cursor) in
  let checksum ~cursor:_ = Optint.zero in
  let is_base ~pos = Hashtbl.find_opt based pos in
  let children ~cursor ~uid =
    let boff = Hashtbl.find_opt (fst child) cursor in
    let bptr = Hashtbl.find_opt (snd child) uid in
    match (boff, bptr) with
    | Some (_ :: _ as children), (Some [] | None) -> children
    | (Some [] | None), Some (_ :: _ as children) -> children
    | (None | Some []), (None | Some []) -> []
    | Some l0, Some l1 -> List.sort_uniq Int.compare (List.rev_append l0 l1)
  in
  let oracle =
    {
      Carton.identify = Carton.Identify Git.identify;
      children;
      where;
      size;
      checksum;
      is_base;
      number_of_objects;
      hash = String.empty;
    }
  in
  Fiber.return (matrix, oracle)

let map (fd, st) ~pos len =
  let len = Int.min (st.Unix.st_size - pos) len in
  let pos = Int64.of_int pos in
  let barr =
    Unix.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Bigarray.array1_of_genarray barr

let verify ?reporter:(verbose = ignore) ~oracle path matrix =
  let open Fiber in
  let fd = Unix.openfile (Bob_fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
  let st = Unix.stat (Bob_fpath.to_string path) in
  let cache = Cachet.make ~map (fd, st) in
  let z = De.bigstring_create 0x7ff in
  let allocate bits = De.make_window ~bits in
  let ref_length = SHA1.length in
  let index _ = assert false in
  let pack = Carton.of_cache cache ~z ~allocate ~ref_length index in
  let on _ _ = verbose () in
  Bob_carton.verify ~on pack oracle matrix >>= fun () ->
  Unix.close fd;
  Fiber.return ()

type unpacked =
  string * int * Carton.Uid.t * (Unix.file_descr * Unix.stats) Carton.t

let unpack path status =
  let is_root = function
    | Carton.Resolved_node { kind = `A; _ }
    | Carton.Resolved_base { kind = `A; _ } ->
        true
    | _ -> false
  in
  match Array.find_opt is_root status with
  | None -> Fiber.return (Error `No_root)
  | Some root ->
      let filename = Bob_fpath.to_string path in
      let fd = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
      let st = Unix.stat filename in
      let id =
        let record tbl = function
          | Carton.Resolved_base { cursor; uid; _ }
          | Carton.Resolved_node { cursor; uid; _ } ->
              Hashtbl.add tbl uid cursor;
              tbl
          | _ -> tbl
        in
        Array.fold_left record (Hashtbl.create 0x100) status
      in
      let index uid = Hashtbl.find id uid in
      let cache = Cachet.make ~map (fd, st) in
      let z = De.bigstring_create 0x7ff in
      let allocate bits = De.make_window ~bits in
      let ref_length = SHA1.length in
      let pack = Carton.of_cache cache ~z ~allocate ~ref_length index in
      let root =
        let cursor =
          match root with
          | Carton.Resolved_node { cursor; _ }
          | Carton.Resolved_base { cursor; _ } ->
              cursor
          | _ -> assert false
        in
        let size = Carton.size_of_offset pack ~cursor Carton.Size.zero in
        let blob = Carton.Blob.make ~size in
        Carton.of_offset pack blob ~cursor
      in
      let root =
        let off = 0
        and len = Carton.Value.length root
        and bstr = Carton.Value.bigstring root in
        bigstring_to_string (Bigarray.Array1.sub bstr off len)
      in
      let[@warning "-8"] (name :: rest) = String.split_on_char '\000' root in
      let rest = String.concat "\000" rest in
      let hash = Carton.Uid.unsafe_of_string (String.sub rest 0 SHA1.length) in
      let total =
        int_of_string
          (String.sub rest SHA1.length (String.length rest - SHA1.length))
      in
      Fiber.return (Ok (name, total, hash, pack))
