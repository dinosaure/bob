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
  rstore : (Fpath.t, SHA256.t * [ `Dir | `Reg | `Root ]) Hashtbl.t;
  root : (SHA256.t * SHA256.t) option;
  path : Fpath.t;
}

let length { rstore; _ } =
  let module Set = Set.Make (SHA256) in
  let hashes =
    Hashtbl.fold (fun _ (hash, _) set -> Set.add hash set) rstore Set.empty
  in
  Set.cardinal hashes

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

let rec full_write ~path fd bstr off len =
  let open Fiber in
  Fiber.write fd bstr ~off ~len >>= function
  | Ok len' when len' - len = 0 -> Fiber.return fd
  | Ok len' -> full_write ~path fd bstr (off + len') (len - len')
  | Error `Closed ->
      Fmt.failwith "Unexpected closed fd %d (%a)" (Obj.magic fd) Fpath.pp path
  | Error (`Unix errno) ->
      Fmt.failwith "write(%d|%a): %s" (Obj.magic fd) Fpath.pp path
        (Unix.error_message errno)

(* XXX(dinosaure): [mmap] can be a solution here but it seems that the kernel
   does many page-faults if the usage is load many files. [mmap] becomes
   interesting when we load several times the same file at different offsets
   (cc XXX(rand)). We will stay simple and see how we can improve that. *)
let load_file path =
  let open Fiber in
  Log.debug (fun m -> m "Load the file: %a." Fpath.pp path);
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
  let open Stream in
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

let load_directory rstore ~root path =
  Log.debug (fun m -> m "Load directory: %a." Fpath.pp path);
  let entries = Filesystem.readdir Fpath.(root // path) in
  let entries =
    List.filter_map
      (fun entry ->
        let key = Fpath.(path / entry) in
        let key = Fpath.relativize ~root key in
        Log.debug (fun m ->
            m "relativize root:%a %a" Fpath.pp root Fpath.pp path);
        let key =
          match key with Some key -> key | None -> Fpath.(path / entry)
        in
        Log.debug (fun m -> m "Try to find: %a." Fpath.pp key);
        match Hashtbl.find_opt rstore key with
        | Some (hash, `Dir) -> Some (Fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Fpath.v entry, hash)
        | Some (_, `Root) -> None
        | None -> None)
      entries
  in
  let open Fiber in
  let open Stream in
  Stream.to_string (serialize_directory entries) >>| fun str ->
  Carton.Dec.v ~kind:`B
    (bigstring_of_string str ~off:0 ~len:(String.length str))

let load_root ~real_length path hash =
  let basename = Fpath.basename path in
  let entry =
    Fmt.str "%s\000%s%d" basename (SHA256.to_raw_string hash) real_length
  in
  Fiber.return
    (Carton.Dec.v ~kind:`A
       (bigstring_of_string entry ~off:0 ~len:(String.length entry)))

let load : store -> SHA256.t -> (Carton.Dec.v, Scheduler.t) Carton.io =
 fun store hash ->
  match (Hashtbl.find_opt store.store hash, store.root) with
  | None, Some (hash_of_root, hash_of_tree) when hash_of_root = hash ->
      Log.debug (fun m -> m "Load root object.");
      let real_length = Hashtbl.length store.rstore in
      Scheduler.inj (load_root ~real_length store.path hash_of_tree)
  | Some path, _ -> (
      let real_path = Fpath.(store.path // path) in
      let stat = Unix.stat Fpath.(to_string real_path) in
      Log.debug (fun m ->
          m "Load %a object (%a)." Fpath.pp path Fpath.pp real_path);
      match stat.Unix.st_kind with
      | Unix.S_REG -> Scheduler.inj (load_file real_path)
      | Unix.S_DIR ->
          Scheduler.inj (load_directory store.rstore ~root:store.path path)
      | _ -> failwith "Invalid kind of object")
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
      Log.debug (fun m -> m "Start to encode a PACK file.");
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
      let anchor = t.cursor in
      Hashtbl.add t.offsets (Carton.Enc.target_uid target) t.cursor;
      let find hash =
        match Hashtbl.find_opt t.offsets hash with
        | Some v ->
            Log.debug (fun m ->
                m "Ask where is %a: %08Lx (anchor: %08Lx)." SHA256.pp hash v
                  anchor);
            Fiber.return (Some (Int64.to_int v))
        | None ->
            Log.err (fun m -> m "%a not found." SHA256.pp hash);
            Fiber.return None
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

let hash_of_directory ~root rstore path =
  let entries = Filesystem.readdir path in
  let entries =
    List.filter_map
      (fun entry ->
        let key = Fpath.(path / entry) in
        let key = Option.get (Fpath.relativize ~root key) in
        match Hashtbl.find_opt rstore key with
        | Some (hash, `Dir) -> Some (Fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Fpath.v entry, hash)
        | Some (_, `Root) -> None
        | None -> None)
      entries
  in
  let open Fiber in
  let open Stream in
  Stream.to_string (serialize_directory entries) >>= fun str ->
  Log.debug (fun m -> m "Serialization of %a:" Fpath.pp path);
  Log.debug (fun m -> m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) str);
  let hdr = Fmt.str "tree %d\000" (String.length str) in
  Stream.(into (SHA256.sink_string ()) (double hdr str))

let hash_of_root ~real_length ~root hash =
  let str =
    Fmt.str "%s\000%s%d" (Fpath.basename root)
      (SHA256.to_raw_string hash)
      real_length
  in
  let hdr = Fmt.str "commit %d\000" (String.length str) in
  SHA256.digest_string (hdr ^ str)

let store root =
  let open Fiber in
  let rstore : (Fpath.t, SHA256.t * [ `Reg | `Dir | `Root ]) Hashtbl.t =
    Hashtbl.create 0x100
  in
  let compute path =
    Log.debug (fun m -> m "Compute %a." Fpath.pp path);
    let stat = Unix.stat (Fpath.to_string path) in
    match stat.Unix.st_kind with
    | Unix.S_REG ->
        hash_of_filename path >>= fun hash ->
        let path = Stdlib.Option.get (Fpath.relativize ~root path) in
        Log.debug (fun m -> m "%a -> %a" Fpath.pp path SHA256.pp hash);
        Hashtbl.add rstore path (hash, `Reg);
        Fiber.return (Some hash)
    | Unix.S_DIR ->
        Log.debug (fun m -> m "Calculate the hash of %a" Fpath.pp path);
        hash_of_directory ~root rstore path >>= fun hash ->
        let path = Stdlib.Option.get (Fpath.relativize ~root path) in
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
  let store = Hashtbl.create (Hashtbl.length rstore) in
  Hashtbl.iter (fun v (k, _) -> Hashtbl.add store k v) rstore;
  let hashes_for_directory =
    if Sys.is_directory (Fpath.to_string root) then (
      let hash_of_tree, _ = Hashtbl.find rstore root in
      let real_length = Hashtbl.length rstore in
      let hash_of_root = hash_of_root ~real_length ~root hash_of_tree in
      Log.debug (fun m -> m "Hash of root: %a" SHA256.pp hash_of_root);
      Log.debug (fun m ->
          m "Hash of tree: %a (%a)" SHA256.pp hash_of_tree Fpath.pp root);
      Hashtbl.add rstore (Fpath.v "./") (hash_of_root, `Root);
      Some (hash_of_root, hash_of_tree))
    else None
  in
  let hashes =
    match hashes_for_directory with
    | Some (hash_of_root, _) -> hash_of_root :: hashes
    | None -> hashes
  in
  let module Set = Set.Make (SHA256) in
  let hashes =
    List.fold_left (fun set hash -> Set.add hash set) Set.empty hashes
  in
  let hashes = Set.elements hashes in
  Log.debug (fun m -> m "Store of %a." Fpath.pp root);
  Fiber.return
    ( Stream.of_list hashes,
      { store; rstore; root = hashes_for_directory; path = root } )

let make ?level ~reporter store =
  let length = length store in
  pack ~reporter ?level ~length store

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
  let entry = Fpath.basename path in
  let length = String.length entry in
  let output =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout (3 * length)
  in
  match
    Zl.Def.Ns.deflate ?level
      (bigstring_of_string entry ~off:0 ~len:length)
      output
  with
  | Ok len ->
      let hdr_entry = encode_header_of_entry ~kind:_D ~length in
      ( bigstring_of_string hdr_entry ~off:0 ~len:(String.length hdr_entry),
        Bigarray.Array1.sub output 0 len )
  | _ -> Fmt.failwith "Impossible to compress the entry with filename"

let make_one ?(level = 4) ~reporter ~finalise path =
  let open Fiber in
  Stream.Stream.of_file path >>= function
  | Error _ as err -> Fiber.return err
  | Ok file ->
      let hdr = Fmt.str "PACK\000\000\000\002\000\000\000\002" in
      let hdr = hdr in
      let hdr = bigstring_of_string hdr ~off:0 ~len:(String.length hdr) in
      let ctx = ref (SHA256.feed_bigstring SHA256.empty hdr) in
      let q = De.Queue.create 0x1000 in
      let w = De.Lz77.make_window ~bits:15 in
      let open Stream in
      let hdr_name, name = entry_with_filename ~level path in
      let hdr_file =
        encode_header_of_entry ~kind:_C
          ~length:(Unix.stat (Fpath.to_string path)).Unix.st_size
        |> fun str -> bigstring_of_string str ~off:0 ~len:(String.length str)
      in
      let zlib = Flow.deflate_zlib ~q ~w ~level in
      let file = Stream.tap (reporter <.> Bigarray.Array1.dim) file in
      let file = Stream.via zlib file in
      let name =
        Stream.tap
          (fun bstr ->
            ctx := SHA256.feed_bigstring !ctx bstr;
            Fiber.return ())
          (Stream.double hdr_name name)
      in
      let file =
        Stream.tap
          (fun bstr ->
            ctx := SHA256.feed_bigstring !ctx bstr;
            Fiber.return ())
          Stream.(singleton hdr_file ++ file)
      in
      Stream.(
        singleton hdr ++ name ++ file
        ++ of_fiber (fun () ->
               finalise ();
               (Fiber.return
               <.> bigstring_of_string ~off:0 ~len:SHA256.length
               <.> SHA256.to_raw_string <.> SHA256.get)
                 !ctx))
      |> fun stream -> Fiber.return (Ok stream)

module First_pass = Carton.Dec.Fp (SHA256)
module Verify = Carton.Dec.Verify (SHA256) (Scheduler) (Fiber)

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

let is_base = Verify.is_base
let is_resolved = Verify.is_resolved
let offset_of_status = Verify.offset_of_status
let uid_of_status = Verify.uid_of_status
let kind_of_status = Verify.kind_of_status

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
      | { First_pass.kind = Base kind; offset; size; _ } ->
          let idx = First_pass.count decoder - 1 in
          Log.debug (fun m ->
              m "[%4d] Got a new entry (base): %08Lx" idx offset);
          let elt =
            (offset, Verify.unresolved_base ~cursor:offset, `Base (kind, size))
          in
          let src = ((First_pass.decode decoder :> decode), src, source) in
          reporter 1 >>| fun () -> `Ret (Some (elt, src))
      | { kind = Ofs { sub; source = s; target }; size; offset; _ } ->
          let idx = First_pass.count decoder - 1 in
          Log.debug (fun m ->
              m "[%4d] Got a new entry (ofs): %08Lx (sub: %08x)" idx offset sub);
          let elt =
            (offset, Verify.unresolved_node, `Ofs (sub, s, target, size))
          in
          let src = ((First_pass.decode decoder :> decode), src, source) in
          reporter 1 >>| fun () -> `Ret (Some (elt, src))
      | { kind = Ref { ptr; source = s; target }; size; offset; _ } ->
          let idx = First_pass.count decoder - 1 in
          Log.debug (fun m ->
              m "[%04x] Got a new entry (ref): %08Lx" idx offset);
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
    let full (_, _, acc) = k.full acc in
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

let digest ~kind ?(off = 0) ?len buf =
  let len =
    match len with Some len -> len | None -> Bigarray.Array1.dim buf - off
  in
  let ctx = SHA256.empty in
  let ctx =
    match kind with
    | `A -> SHA256.feed_string ctx (Fmt.str "commit %d\000" len)
    | `B -> SHA256.feed_string ctx (Fmt.str "tree %d\000" len)
    | `C -> SHA256.feed_string ctx (Fmt.str "blob %d\000" len)
    | `D -> SHA256.feed_string ctx (Fmt.str "mesg %d\000" len)
  in
  let ctx = SHA256.feed_bigstring ctx ~off ~len buf in
  SHA256.get ctx

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
            m "An object references to another one: %a" SHA256.pp ptr);
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
  let oracle = { Carton.Dec.where; children; digest; weight } in
  Fiber.return (matrix, oracle)

let verify ?reporter:(verbose = ignore) ~oracle path matrix =
  let open Fiber in
  let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
  let st = Unix.LargeFile.stat (Fpath.to_string path) in
  let pack =
    Carton.Dec.make (fd, st) ~allocate:make_window
      ~z:(De.bigstring_create De.io_buffer_size)
      ~uid_ln:SHA256.length ~uid_rw:SHA256.of_raw_string Stdbob.never
  in
  Verify.verify ~threads:4 pack ~map ~oracle ~verbose ~matrix >>= fun () ->
  Unix.close fd;
  Fiber.return ()

let v_space = Cstruct.string " "
let v_null = Cstruct.string "\x00"

let readdir ~path contents =
  let init () = Fiber.return (Cstruct.of_bigarray contents) in
  let pull contents =
    let ( >>= ) = Option.bind in
    Cstruct.cut ~sep:v_space contents >>= fun (perm, contents) ->
    Cstruct.cut ~sep:v_null contents >>= fun (name, contents) ->
    (try Some (Cstruct.sub contents 0 SHA256.length) with _ -> None)
    >>= fun hash ->
    let contents = Cstruct.shift contents SHA256.length in
    let hash = SHA256.of_raw_string (Cstruct.to_string hash) in
    match Cstruct.to_string perm with
    | "40000" ->
        Some (`Dir (Fpath.(path / Cstruct.to_string name), hash), contents)
    | "100644" ->
        Some (`Reg (Fpath.(path / Cstruct.to_string name), hash), contents)
    | _ -> failwith "Invalid kind of entry into a tree"
  in
  let pull = Fiber.return <.> pull in
  let stop = Fiber.ignore in
  Stream.Source { init; pull; stop }

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
  if not (Sys.file_exists (Fpath.to_string path)) then
    Sys.mkdir (Fpath.to_string path) 0o755;
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
  Stream.Stream.run ~from:(readdir ~path contents) ~via:Stream.Flow.identity
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
          m "Impossible to save %a: %s" Fpath.pp path (Unix.error_message errno));
      Fiber.return pack
  | Ok fd ->
      let open Fiber in
      full_write ~path fd bstr 0 (Bigarray.Array1.dim bstr) >>= Fiber.close
      >>= fun () -> Fiber.return pack

let extract_file ~file ~name path =
  let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
  let st = Unix.LargeFile.stat (Fpath.to_string path) in
  let pack =
    Carton.Dec.make (fd, st) ~allocate:make_window
      ~z:(De.bigstring_create De.io_buffer_size)
      ~uid_ln:SHA256.length ~uid_rw:SHA256.of_raw_string Stdbob.never
  in
  let name_ofs = Verify.offset_of_status name in
  let name =
    let weight =
      Carton.Dec.weight_of_offset ~map pack ~weight:Carton.Dec.null name_ofs
    in
    let raw = Carton.Dec.make_raw ~weight in
    Carton.Dec.of_offset ~map pack raw ~cursor:name_ofs
  in
  let name =
    bigstring_to_string
      (Bigarray.Array1.sub (Carton.Dec.raw name) 0 (Carton.Dec.len name))
  in
  let file_ofs = Verify.offset_of_status file in
  (* TODO(dinosaure): we should have a streaming API for a /base/
     object. At least, we will be able to show that we save the file
     interactively into the file-system. *)
  let file =
    let weight =
      Carton.Dec.weight_of_offset ~map pack ~weight:Carton.Dec.null file_ofs
    in
    let raw = Carton.Dec.make_raw ~weight in
    Carton.Dec.of_offset ~map pack raw ~cursor:file_ofs
  in
  let open Fiber in
  let open Stream in
  Stream.to_file (Fpath.v name)
    (Stream.singleton
       (Bigarray.Array1.sub (Carton.Dec.raw file) 0 (Carton.Dec.len file)))
  >>= fun res ->
  Unix.close fd;
  Fiber.return res
