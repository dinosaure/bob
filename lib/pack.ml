open Stdbob

let src = Logs.Src.create "bob.pack"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA256 = struct
  include Digestif.SHA1

  let hash x = Hashtbl.hash x
  let length = digest_size
  let feed = feed_bigstring
  let null = digest_string ""
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)

  let sink_bigstring ?(ctx = empty) () =
    Stream.Sink.make ~init:(Fiber.always ctx)
      ~push:(fun ctx bstr -> Fiber.return (feed_bigstring ctx bstr))
      ~stop:(Fiber.return <.> get) ()

  let sink_string ?(ctx = empty) () =
    Stream.Sink.make ~init:(Fiber.always ctx)
      ~push:(fun ctx str -> Fiber.return (feed_string ctx str))
      ~stop:(Fiber.return <.> get) ()
end

module Crc32 = Checkseum.Crc32
module Scheduler = Carton.Make (Fiber)

let scheduler =
  let open Fiber in
  let open Scheduler in
  {
    Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    Carton.return = (fun x -> inj (return x));
  }

type store = {
  store : (SHA256.t, Fpath.t) Hashtbl.t;
  rstore : (Fpath.t, SHA256.t * [ `Dir | `Reg ]) Hashtbl.t;
  root : SHA256.t option;
  path : Fpath.t;
}

let length { rstore; root; _ } =
  match root with
  | Some _ -> Hashtbl.length rstore + 1
  | None -> Hashtbl.length rstore

module Filesystem = struct
  let readdir =
    let readdir d = try Sys.readdir (Fpath.to_string d) with _exn -> [||] in
    Array.to_list <.> readdir

  let rec traverse ~get ~add visited stack ~f acc =
    match stack with
    | [] -> Fiber.return acc
    | x :: r ->
        if List.exists (Fpath.equal x) visited then
          traverse ~get ~add visited r ~f acc
        else
          let open Fiber in
          let contents = get x in
          traverse ~get ~add (x :: visited) (add contents stack) ~f acc >>= f x

  let fold ?(dotfiles = false) ~f acc paths =
    let dir_child d acc bname =
      if (not dotfiles) && bname.[0] = '.' then acc
      else Fpath.(d / bname) :: acc
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
let load_file path =
  let open Fiber in
  Log.debug (fun m -> m "Load the file: %a.\n%!" Fpath.pp path);
  let len = (Unix.stat (Fpath.to_string path)).Unix.st_size in
  Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
  | Ok fd ->
      let res = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
      full_read fd res 0 len;
      Fiber.close fd >>| fun () -> Carton.Dec.v ~kind:`C res
  | Error errno ->
      Fmt.failwith "openfile(%a): %s" Fpath.pp path (Unix.error_message errno)

let serialize_directory entries =
  (* XXX(dinosaure): à la Git. *)
  let entries = List.sort (fun (a, _) (b, _) -> Fpath.compare a b) entries in
  let open Stream in
  let open Stream.Infix in
  Stream.of_list entries >>= fun (p, hash) ->
  match Fpath.is_dir_path p with
  | true ->
      Stream.of_list
        [
          "40000 ";
          Fpath.(to_string (rem_empty_seg p));
          "\x00";
          SHA256.to_raw_string hash;
        ]
      |> Fiber.return
  | false ->
      Stream.of_list
        [ "100644 "; Fpath.to_string p; "\x00"; SHA256.to_raw_string hash ]
      |> Fiber.return

let load_directory rstore path =
  let entries = Filesystem.readdir path in
  let entries =
    List.filter_map
      (fun entry ->
        match Hashtbl.find_opt rstore Fpath.(path / entry) with
        | Some (hash, `Dir) -> Some (Fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Fpath.v entry, hash)
        | None -> None)
      entries
  in
  let open Fiber in
  let open Stream in
  Stream.to_string (serialize_directory entries) >>| fun str ->
  Carton.Dec.v ~kind:`B
    (bigstring_of_string str ~off:0 ~len:(String.length str))

let load_root hash =
  let str = Fmt.str "commit 30\000%s" (SHA256.to_raw_string hash) in
  Fiber.return
    (Carton.Dec.v ~kind:`A
       (bigstring_of_string str ~off:0 ~len:(String.length str)))

let load : store -> SHA256.t -> (Carton.Dec.v, Scheduler.t) Carton.io =
 fun store hash ->
  match (Hashtbl.find_opt store.store hash, store.root) with
  | Some path, _ -> (
      let path = Fpath.(store.path // path) in
      let stat = Unix.stat Fpath.(to_string path) in
      match stat.Unix.st_kind with
      | Unix.S_REG -> Scheduler.inj (load_file path)
      | Unix.S_DIR -> Scheduler.inj (load_directory store.rstore path)
      | _ -> failwith "Invalid kind of object")
  | None, Some hash' when SHA256.equal hash hash' ->
      Scheduler.inj (load_root hash')
  | None, (Some _ | None) ->
      Log.err (fun m -> m "The object %a does not exists." SHA256.pp hash);
      raise Not_found

let deltify ~reporter ?(compression = true) store hashes =
  let open Fiber in
  match compression with
  | true ->
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
      let module Delta = Carton.Enc.Delta (Scheduler) (Fiber) (SHA256) (Verbose)
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
        ~weight:10 ~uid_ln:SHA256.length entries
      >>| Stream.Stream.of_array
  | false ->
      (* XXX(dinosaure): just generate targets without patch compression. *)
      let f hash =
        load store hash |> Scheduler.prj >>= fun v ->
        let kind = Carton.Dec.kind v in
        let length = Carton.Dec.len v in
        let entry = Carton.Enc.make_entry ~kind ~length hash in
        Carton.Enc.entry_to_target scheduler
          ~load:(fun _ -> Scheduler.inj (Fiber.return v))
          entry
        |> Scheduler.prj
        >>= fun target ->
        reporter 0 >>= fun () -> Fiber.return target
      in
      let open Stream in
      Fiber.return (Stream.map f hashes)

type t = {
  mutable ctx : SHA256.ctx;
  mutable cursor : int64;
  b : Carton.Enc.b;
  uid : SHA256.t Carton.Enc.uid;
  offsets : (SHA256.t, int64) Hashtbl.t;
}

let rec encode_target t ~push ~acc encoder =
  let open Fiber in
  match Carton.Enc.N.encode ~o:t.b.o encoder with
  | `Flush (encoder, len) ->
      push acc (Bigarray.Array1.sub t.b.o 0 len) >>= fun acc ->
      t.ctx <- SHA256.feed_bigstring t.ctx t.b.o ~off:0 ~len;
      t.cursor <- Int64.add t.cursor (Int64.of_int len);
      let encoder =
        Carton.Enc.N.dst encoder t.b.o 0 (Bigarray.Array1.dim t.b.o)
      in
      encode_target t ~push ~acc encoder
  | `End -> Fiber.return acc

let pack ~reporter ?level ~length store =
  let flow (Stream.Sink k) =
    let init () =
      let uid =
        {
          Carton.Enc.uid_ln = SHA256.digest_size;
          Carton.Enc.uid_rw = SHA256.to_raw_string;
        }
      in
      let b =
        {
          Carton.Enc.o =
            Bigarray.Array1.create Bigarray.char Bigarray.c_layout
              io_buffer_size;
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
      let ctx = SHA256.empty in
      Carton.Enc.header_of_pack ~length hdr 0 12;
      let ctx = SHA256.feed_bigstring ctx hdr ~off:0 ~len:12 in
      k.push acc hdr >>= fun acc ->
      Fiber.return
        ({ ctx; cursor = 12L; b; uid; offsets = Hashtbl.create length }, acc)
    in
    let push (t, acc) target =
      let open Fiber in
      reporter () >>= fun () ->
      Hashtbl.add t.offsets (Carton.Enc.target_uid target) t.cursor;
      let find hash =
        match Hashtbl.find_opt t.offsets hash with
        | Some v -> Fiber.return (Some (Int64.to_int v))
        | None -> Fiber.return None
      in

      Log.debug (fun m ->
          m "Start to encode %a at %08Lx" SHA256.pp
            (Carton.Enc.target_uid target)
            t.cursor);
      Carton.Enc.encode_target ?level scheduler ~b:t.b
        ~find:(Scheduler.inj <.> find) ~load:(load store) ~uid:t.uid target
        ~cursor:(Int64.to_int t.cursor)
      |> Scheduler.prj
      >>= fun (len, encoder) ->
      k.push acc (Bigarray.Array1.sub t.b.o 0 len) >>= fun acc ->
      t.ctx <- SHA256.feed_bigstring t.ctx t.b.o ~off:0 ~len;
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
      let hash = SHA256.get t.ctx in
      Log.debug (fun m -> m "Hash of the PACK file: %a" SHA256.pp hash);
      let hash = SHA256.to_raw_string hash in
      let hash = bigstring_of_string hash ~off:0 ~len:(String.length hash) in
      k.push acc hash >>= k.stop
    in
    Stream.Sink { init; stop; full; push }
  in
  { Stream.flow }

let hash_of_filename path =
  let open Fiber in
  let open Stream in
  let len = Unix.(stat (Fpath.to_string path)).Unix.st_size in
  let hdr = Fmt.str "blob %d\000" len in
  let ctx = SHA256.feed_string SHA256.empty hdr in
  Stream.of_file path >>= function
  | Error (`Msg err) -> Fmt.failwith "%s." err
  | Ok stream -> Stream.(into (SHA256.sink_bigstring ~ctx ()) stream)

let hash_of_directory rstore path =
  let entries = Filesystem.readdir path in
  let entries =
    List.filter_map
      (fun entry ->
        match Hashtbl.find_opt rstore Fpath.(path / entry) with
        | Some (hash, `Dir) -> Some (Fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Fpath.v entry, hash)
        | None -> None)
      entries
  in
  let open Fiber in
  let open Stream in
  Stream.to_string (serialize_directory entries) >>= fun str ->
  let hdr = Fmt.str "tree %d\000" (String.length str) in
  Stream.(into (SHA256.sink_string ()) (double hdr str))

let store root =
  let open Fiber in
  let rstore = Hashtbl.create 0x100 in
  let compute path =
    let stat = Unix.stat (Fpath.to_string path) in
    match stat.Unix.st_kind with
    | Unix.S_REG ->
        hash_of_filename path >>= fun hash ->
        let path = Option.get (Fpath.relativize ~root path) in
        Log.debug (fun m -> m "%a -> %a" Fpath.pp path SHA256.pp hash);
        Hashtbl.add rstore path (hash, `Reg);
        Fiber.return (Some hash)
    | Unix.S_DIR ->
        let path = Fpath.to_dir_path path in
        hash_of_directory rstore path >>= fun hash ->
        let path = Option.get (Fpath.relativize ~root path) in
        Log.debug (fun m -> m "%a -> %a" Fpath.pp path SHA256.pp hash);
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
  let hash_of_root, path =
    if Sys.is_directory (Fpath.to_string root) then
      let hash, _ = Hashtbl.find rstore (Fpath.v "./") in
      (Some hash, Fpath.to_dir_path root)
    else (None, root)
  in
  let store = Hashtbl.create (Hashtbl.length rstore) in
  Hashtbl.iter (fun v (k, _) -> Hashtbl.add store k v) rstore;
  Log.debug (fun m ->
      m "The store contains %d objects." (Hashtbl.length rstore));
  let hashes =
    match hash_of_root with Some hash -> hash :: hashes | None -> hashes
  in
  Fiber.return
    (Stream.of_list hashes, { store; rstore; root = hash_of_root; path })

let make ?level ~reporter store =
  let length = length store in
  pack ~reporter ?level ~length store

let encode_header_of_file ~length =
  let buf = Buffer.create 16 in
  let c = ref ((0b011 (* [`C] *) lsl 4) lor (length land 15)) in
  let l = ref (length asr 4) in
  while !l != 0 do
    Buffer.add_char buf (Char.chr (!c lor 0x80 land 0xff));
    c := !l land 0x7f;
    l := !l asr 7
  done;
  Buffer.add_char buf (Char.chr !c);
  Buffer.contents buf

let make_one ?(level = 4) ~reporter path =
  let open Fiber in
  Stream.Stream.of_file path >>= function
  | Error _ as err -> Fiber.return err
  | Ok file ->
      let hdr = Fmt.str "PACK\000\000\000\002\000\000\000\001" in
      let hdr_entry =
        encode_header_of_file
          ~length:(Unix.stat (Fpath.to_string path)).Unix.st_size
      in
      let hdr = hdr ^ hdr_entry in
      let hdr = bigstring_of_string hdr ~off:0 ~len:(String.length hdr) in
      let ctx = ref SHA256.empty in
      let q = De.Queue.create 0x1000 in
      let w = De.Lz77.make_window ~bits:15 in
      let open Stream in
      let zlib = Flow.deflate_zlib ~q ~w ~level in
      let file =
        Stream.tap
          (fun bstr ->
            ctx := SHA256.feed_bigstring !ctx bstr;
            Fiber.return ())
          file
      in
      let file = Stream.tap (reporter <.> Bigarray.Array1.dim) file in
      let file = Stream.via zlib file in
      Stream.Infix.(
        Stream.singleton hdr ++ file
        ++ Stream.of_fiber (fun () ->
               (Fiber.return
               <.> bigstring_of_string ~off:0 ~len:SHA256.length
               <.> SHA256.to_raw_string <.> SHA256.get)
                 !ctx))
      |> fun stream -> Fiber.return (Ok stream)

module First_pass = Carton.Dec.Fp (SHA256)
module Verify = Carton.Dec.Verify (SHA256) (Scheduler) (Fiber)

type status = Verify.status
type decode = [ First_pass.decode | `Keep of First_pass.decode * string ]

let rec analyse ~reporter src source state =
  let open Fiber in
  match state with
  | `Await decoder -> (
      Stream.Source.next source >>= function
      | Some (src', source) ->
          let len = Bigarray.Array1.dim src' in
          analyse ~reporter src' source
            (First_pass.decode (First_pass.src decoder src' 0 len))
      | None ->
          Stream.Source.dispose source >>= fun () ->
          let src' = De.bigstring_empty in
          analyse ~reporter src' source
            (First_pass.decode (First_pass.src decoder src' 0 0)))
  | `Peek decoder -> (
      Stream.Source.next source >>= function
      | Some (src', source) ->
          let src'_len = Bigarray.Array1.dim src'
          and src_off = First_pass.src_rem decoder in
          let len = min src'_len (Bigarray.Array1.dim src - src_off) in
          bigstring_blit src ~src_off src' ~dst_off:0 ~len;
          if src'_len - len = 0 then
            analyse ~reporter src source
              (First_pass.decode (First_pass.src decoder src 0 (src_off + len)))
          else
            let rest =
              bigstring_substring src' ~off:len
                ~len:(Bigarray.Array1.dim src' - len)
            in
            peek ~reporter src rest source
              (First_pass.decode (First_pass.src decoder src 0 (src_off + len)))
      | None ->
          Stream.Source.dispose source >>= fun () ->
          let src' = De.bigstring_empty in
          analyse ~reporter src' source
            (First_pass.decode (First_pass.src decoder src' 0 0)))
  | `End _ -> Fiber.return (`Ret None)
  | `Malformed err -> Fmt.failwith "PACK: %s" err
  | `Entry (entry, decoder) -> (
      match entry with
      | { First_pass.kind = Base _; offset; size; _ } ->
          Log.debug (fun m -> m "Got a new entry (base): %08Lx" offset);
          let elt =
            (offset, Verify.unresolved_base ~cursor:offset, `Base size)
          in
          let src = ((First_pass.decode decoder :> decode), src, source) in
          reporter 1 >>| fun () -> `Ret (Some (elt, src))
      | { kind = Ofs { sub; source = s; target }; size; offset; _ } ->
          Log.debug (fun m -> m "Got a new entry (ofs): %08Lx" offset);
          let elt =
            (offset, Verify.unresolved_node, `Ofs (sub, s, target, size))
          in
          let src = ((First_pass.decode decoder :> decode), src, source) in
          reporter 1 >>| fun () -> `Ret (Some (elt, src))
      | { kind = Ref { ptr; source = s; target }; size; offset; _ } ->
          Log.debug (fun m -> m "Got a new entry (ref): %08Lx" offset);
          let elt =
            (offset, Verify.unresolved_node, `Ref (ptr, s, target, size))
          in
          let src = ((First_pass.decode decoder :> decode), src, source) in
          reporter 1 >>| fun () -> `Ret (Some (elt, src)))

and peek ~reporter src rest source = function
  | `Await decoder ->
      let src' = bigstring_of_string rest ~off:0 ~len:(String.length rest) in
      analyse ~reporter src' source
        (First_pass.decode (First_pass.src decoder src' 0 (String.length rest)))
  | `Peek decoder ->
      let src'_len = String.length rest
      and src_off = First_pass.src_rem decoder in
      Log.debug (fun m -> m "Peek %d bytes from the given flow." src'_len);
      let len = min src'_len (Bigarray.Array1.dim src - src_off) in
      bigstring_blit_from_string rest ~src_off:0 src ~dst_off:src_off ~len;
      if String.length rest - len = 0 then
        analyse ~reporter src source
          (First_pass.decode (First_pass.src decoder src 0 (src_off + len)))
      else
        let rest = String.sub rest len (String.length rest - len) in
        peek ~reporter src rest source
          (First_pass.decode (First_pass.src decoder src 0 (src_off + len)))
  | state ->
      let open Fiber in
      analyse ~reporter src source state >>= fun v ->
      Fiber.return (`Keep (rest, v))

let first_pass ~reporter source =
  let open Fiber in
  let init () =
    let oc = De.bigstring_create De.io_buffer_size in
    let zw = De.make_window ~bits:15 in
    let allocate _ = zw in
    let decoder = First_pass.decoder ~o:oc ~allocate `Manual in
    Fiber.return (`Await decoder, De.bigstring_empty, source)
  in
  let pull (state, src, source) =
    match state with
    | `Keep (state, rest) -> (
        peek ~reporter src rest source state >>= function
        | `Ret v -> Fiber.return v
        | `Keep (_, `Ret None) -> Fiber.return None
        | `Keep (r, `Ret (Some (elt, ((#First_pass.decode as s), src, source))))
          ->
            Fiber.return (Some (elt, (`Keep (s, r), src, source)))
        | `Keep _ -> assert false)
    | `End _ -> Fiber.return None
    | #First_pass.decode as state -> (
        analyse ~reporter src source state >>= function
        | `Ret v -> Fiber.return v
        | `Keep (_, `Ret None) -> Fiber.return None
        | `Keep (r, `Ret (Some (elt, ((#First_pass.decode as s), src, source))))
          ->
            Fiber.return (Some (elt, (`Keep (s, r), src, source)))
        | `Keep _ -> assert false)
  in
  let stop (_, _, source) =
    (* TODO(dinosaure): We probably should check if we nicely terminates with [`End]. *)
    Stream.Source.dispose source
  in
  Stream.Source { init; pull; stop }
