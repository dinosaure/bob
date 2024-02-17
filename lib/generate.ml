open Stdbob

let src = Logs.Src.create "bob.generate"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA1 = struct
  include Digestif.SHA1

  let hash x = Hashtbl.hash x
  let length = digest_size
  let null = digest_string ""
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
end

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

module Scheduler = Carton.Make (Fiber)

let scheduler =
  let open Fiber in
  let open Scheduler in
  {
    Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
    Carton.return = (fun x -> inj (return x));
  }

type store = {
  store : (SHA1.t, Bob_fpath.t) Hashtbl.t;
  rstore : (Bob_fpath.t, SHA1.t * [ `Dir | `Reg | `Root ]) Hashtbl.t;
  root : SHA1.t * SHA1.t;
  path : Bob_fpath.t;
}

let length { rstore; _ } =
  let module Set = Set.Make (SHA1) in
  let hashes =
    Hashtbl.fold (fun _ (hash, _) set -> Set.add hash set) rstore Set.empty
  in
  Set.cardinal hashes

external bigstring_read :
  Unix.file_descr -> Stdbob.bigstring -> int -> int -> int
  = "bob_bigstring_read"
[@@noalloc]

let rec full_read fd ba off len =
  if len > 0 then
    match bigstring_read fd ba off len with
    | ret when ret < 0 ->
        Log.err (fun m -> m "Got an error while reading %d" (Obj.magic fd));
        Fmt.failwith "Got an error while reading %d" (Obj.magic fd)
    | len' when len - len' > 0 ->
        Log.debug (fun m -> m "Load %d byte(s) from %d." len' (Obj.magic fd));
        full_read fd ba (off + len') (len - len')
    | _ -> ()

(* XXX(dinosaure): [mmap] can be a solution here but it seems that the kernel
   does many page-faults if the usage is load many files. [mmap] becomes
   interesting when we load several times the same file at different offsets
   (cc XXX(rand)). We will stay simple and see how we can improve that. *)
let load_file path =
  let open Fiber in
  Log.debug (fun m -> m "Load the file: %a." Bob_fpath.pp path);
  let len = (Unix.stat (Bob_fpath.to_string path)).Unix.st_size in
  Fiber.openfile path Unix.[ O_RDONLY ] 0o644 >>= function
  | Ok fd -> (
      let res = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
      try
        full_read fd res 0 len;
        Log.debug (fun m -> m "%a loaded in memory." Bob_fpath.pp path);
        Fiber.close fd >>| fun () -> Carton.Dec.v ~kind:`C res
      with exn -> Fiber.close fd >>= fun () -> raise exn)
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
  Stream.to_string (Git.serialize_directory entries) >>| fun str ->
  Carton.Dec.v ~kind:`B
    (bigstring_of_string str ~off:0 ~len:(String.length str))

let load_root path hash =
  let open Fiber in
  let open Stream in
  let entries = [ (Bob_fpath.base path, hash) ] in
  Stream.to_string (Git.serialize_directory entries) >>| fun str ->
  Log.info (fun m ->
      m "Output of the root object: @[<hov>%a@]" (Hxd_string.pp Hxd.default) str);
  Carton.Dec.v ~kind:`B
    (bigstring_of_string str ~off:0 ~len:(String.length str))

let load : store -> SHA1.t -> (Carton.Dec.v, Scheduler.t) Carton.io =
 fun store hash ->
  match (Hashtbl.find_opt store.store hash, store.root) with
  | None, (hash_of_root, hash_of_tree) when hash_of_root = hash ->
      Log.debug (fun m -> m "Load root object.");
      Scheduler.inj (load_root store.path hash_of_tree)
  | Some path, _ -> (
      let real_path = Bob_fpath.(normalize (store.path // path)) in
      let stat = Unix.stat (Bob_fpath.to_string real_path) in
      Log.debug (fun m ->
          m "Load %a object (%a)." Bob_fpath.pp real_path Bob_fpath.pp path);
      match stat.Unix.st_kind with
      | Unix.S_REG -> Scheduler.inj (load_file real_path)
      | Unix.S_DIR -> Scheduler.inj (load_directory store.rstore real_path)
      | _ -> failwith "Invalid kind of object")
  | None, _ ->
      Log.err (fun m -> m "The object %a does not exists." SHA1.pp hash);
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
      >>= fun arr ->
      let first_entry =
        load store (fst store.root) |> Scheduler.prj >>= fun v ->
        let kind = Carton.Dec.kind v in
        let length = Carton.Dec.len v in
        let entry = Carton.Enc.make_entry ~kind ~length (fst store.root) in
        Carton.Enc.entry_to_target scheduler
          ~load:(fun _ -> Scheduler.inj (Fiber.return v))
          entry
        |> Scheduler.prj
        >>= fun target ->
        reporter 0 >>= fun () -> Fiber.return target
      in
      first_entry >>| fun first_entry ->
      Stream.Stream.of_list (first_entry :: Array.to_list arr)
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
      let stream = Stream.(concat (singleton (fst store.root)) hashes) in
      Fiber.return (Stream.map f stream)

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
  let hash_of_root, hash_of_tree =
    let hash_of_tree, _ = Hashtbl.find rstore root in
    let real_length = Hashtbl.length rstore in
    let hash_of_root = Git.hash_of_root ~real_length ~root hash_of_tree in
    Log.debug (fun m -> m "Hash of root: %a" SHA1.pp hash_of_root);
    Log.debug (fun m ->
        m "Hash of tree: %a (%a)" SHA1.pp hash_of_tree Bob_fpath.pp root);
    Hashtbl.add rstore (Bob_fpath.v "./") (hash_of_root, `Root);
    (hash_of_root, hash_of_tree)
  in
  let module Set = Set.Make (SHA1) in
  let hashes =
    List.fold_left (fun set hash -> Set.add hash set) Set.empty hashes
  in
  let hashes = Set.elements hashes in
  Log.debug (fun m -> m "Store of %a." Bob_fpath.pp root);
  Fiber.return
    ( Stream.of_list hashes,
      {
        store;
        rstore;
        root = (hash_of_root, hash_of_tree);
        path = Bob_fpath.to_dir_path root;
      } )

type pack = {
  mutable ctx : SHA1.ctx;
  mutable cursor : int64;
  b : Carton.Enc.b;
  uid : SHA1.t Carton.Enc.uid;
  offsets : (SHA1.t, int64) Hashtbl.t;
}

let rec encode_target t ~push ~acc encoder =
  let open Fiber in
  match Carton.Enc.N.encode ~o:t.b.o encoder with
  | `Flush (encoder, len) ->
      push acc (Bigarray.Array1.sub t.b.o 0 len) >>= fun acc ->
      t.ctx <- SHA1.feed_bigstring t.ctx t.b.o ~off:0 ~len;
      t.cursor <- Int64.add t.cursor (Int64.of_int len);
      let encoder =
        Carton.Enc.N.dst encoder t.b.o 0 (Bigarray.Array1.dim t.b.o)
      in
      encode_target t ~push ~acc encoder
  | `End -> Fiber.return acc

let pack ?(len = Stdbob.io_buffer_size) ~reporter ?level ~length store =
  let flow (Stream.Sink k) =
    let init () =
      Log.debug (fun m -> m "Start to encode a PACK file.");
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

let make_from_store ?len ?level ~reporter store =
  let length = length store in
  pack ?len ~reporter ?level ~length store

let _A = 0b001
let _B = 0b010
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

let first_entry ?(level = 4) entries =
  let open Fiber in
  let open Stream in
  Stream.to_string (Git.serialize_directory entries) >>= fun tree ->
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
  let deflated =
    let q = De.Queue.create 0x1000 in
    let w = De.Lz77.make_window ~bits:15 in
    let o = De.bigstring_create 0x100 in
    let z = Zl.Def.encoder ~q ~w ~level `Manual `Manual in
    let b = Stdbob.bigstring_of_string tree ~off:0 ~len:(String.length tree) in
    let z = Zl.Def.src z b 0 (String.length tree) in
    let z = Zl.Def.dst z o 0 (De.bigstring_length o) in
    deflate z (Buffer.create 0x100) o
  in
  let tree_len = String.length tree in
  let deflated_len = String.length deflated in
  let deflated = bigstring_of_string deflated ~off:0 ~len:deflated_len in
  let hdr_entry = encode_header_of_entry ~kind:_B ~length:tree_len in
  let hdr_entry_len = String.length hdr_entry in
  let hdr_entry = bigstring_of_string hdr_entry ~off:0 ~len:hdr_entry_len in
  Fiber.return (hdr_entry, deflated)

let make_from_file ?(len = Stdbob.io_buffer_size) ?(level = 4) ~reporter
    ~finalise path =
  let open Fiber in
  Stream.Stream.of_file path >>= function
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
      first_entry ~level [ (path, SHA1.null) ]
      >>= fun (hdr_first_entry, first_entry) ->
      let hdr_file =
        encode_header_of_entry ~kind:_B
          ~length:(Unix.stat (Bob_fpath.to_string path)).Unix.st_size
        |> fun str -> bigstring_of_string str ~off:0 ~len:(String.length str)
      in
      let zlib = Flow.deflate_zlib ~len ~q ~w level in
      let file = Stream.tap (reporter <.> Bigarray.Array1.dim) file in
      let file = Stream.via zlib file in
      let first_entry =
        Stream.tap
          (fun bstr ->
            Log.debug (fun m -> m "Calculate the PACK hash with:");
            Log.debug (fun m ->
                m "@[<hov>%a@]"
                  (Hxd_string.pp Hxd.default)
                  (bigstring_to_string bstr));
            ctx := SHA1.feed_bigstring !ctx bstr;
            Fiber.return ())
          (Stream.double hdr_first_entry first_entry)
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
      Stream.(
        singleton hdr ++ first_entry ++ file
        ++ of_fiber (fun () ->
               finalise ();
               let hash = SHA1.get !ctx in
               Log.debug (fun m -> m "Hash of the PACK file: %a." SHA1.pp hash);
               let hash =
                 (bigstring_of_string ~off:0 ~len:SHA1.length
                 <.> SHA1.to_raw_string)
                   hash
               in
               Fiber.return hash))
      |> fun stream ->
      Log.debug (fun m -> m "The PACK stream is ready to be sent.");
      Fiber.return (Ok stream)
