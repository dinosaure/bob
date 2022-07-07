let src = Logs.Src.create "bob.pack"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA256 = struct
  include Digestif.SHA256

  let hash x = Hashtbl.hash x
  let length = digest_size
  let feed = feed_bigstring
  let null = digest_string ""
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)
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
}

let ( <.> ) f g x = f (g x)

module Filesystem = struct
  let readdir =
    let readdir d = try Sys.readdir (Fpath.to_string d) with _exn -> [||] in
    Array.to_list <.> readdir

  let rec traverse ~get ~add visited stack ~f acc =
    match stack with
    | [] -> acc
    | x :: r ->
        if List.exists (Fpath.equal x) visited then
          traverse ~get ~add visited r ~f acc
        else
          let contents = get x in
          let acc =
            traverse ~get ~add (x :: visited) (add contents stack) ~f acc
          in
          f x acc

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

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  let len' = input ic tmp 0 len in
  Stdbob.bigstring_blit_from_bytes tmp ~src_off:0 buf ~dst_off:off ~len:len';
  len'

let load_file fpath =
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let ln = Unix.lseek fd 0 Unix.SEEK_END in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let bf = Bytes.create ln in
  let _ = Unix.read fd bf 0 ln in
  Unix.close fd;
  let rs = Bigarray.Array1.create Bigarray.char Bigarray.c_layout ln in
  Stdbob.bigstring_blit_from_bytes bf ~src_off:0 rs ~dst_off:0 ~len:ln;
  Carton.Dec.v ~kind:`C rs

let serialize_directory entries =
  (* XXX(dinosaure): à la Git. *)
  let entries = List.sort (fun (a, _) (b, _) -> Fpath.compare a b) entries in
  let buf = Buffer.create (List.length entries * (20 + SHA256.length)) in
  List.iter
    (fun (p, hash) ->
      match Fpath.is_dir_path p with
      | true ->
          Buffer.add_string buf "40000 ";
          Buffer.add_string buf Fpath.(to_string (rem_empty_seg p));
          Buffer.add_char buf '\x00';
          Buffer.add_string buf (SHA256.to_raw_string hash)
      | false ->
          Buffer.add_string buf "100644 ";
          Buffer.add_string buf Fpath.(to_string p);
          Buffer.add_char buf '\x00';
          Buffer.add_string buf (SHA256.to_raw_string hash))
    entries;
  Buffer.contents buf

let load_directory rstore fpath =
  let entries = Filesystem.readdir fpath in
  let entries =
    List.filter_map
      (fun entry ->
        match Hashtbl.find_opt rstore Fpath.(fpath / entry) with
        | Some (hash, `Dir) -> Some (Fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Fpath.v entry, hash)
        | None -> None)
      entries
  in
  let str = serialize_directory entries in
  Carton.Dec.v ~kind:`B
    (Stdbob.bigstring_of_string str ~off:0 ~len:(String.length str))

let load : store -> SHA256.t -> (Carton.Dec.v, Scheduler.t) Carton.io =
 fun store hash ->
  match Hashtbl.find_opt store.store hash with
  | None -> raise Not_found
  | Some fpath -> (
      let stat = Unix.stat (Fpath.to_string fpath) in
      match stat.Unix.st_kind with
      | Unix.S_REG -> Scheduler.inj (Fiber.return (load_file fpath))
      | Unix.S_DIR ->
          Scheduler.inj (Fiber.return (load_directory store.rstore fpath))
      | _ -> failwith "Invalid kind of object")
(* XXX(dinosaure): should we /fiberize/ unix syscalls of [load_file]/[load_directory]? *)

let deltify ~reporter store hashes =
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
  let module Delta = Carton.Enc.Delta (Scheduler) (Fiber) (SHA256) (Verbose) in
  let open Fiber in
  let rec fold acc = function
    | [] -> Fiber.return (List.rev acc)
    | hash :: rest ->
        load store hash |> Scheduler.prj >>= fun v ->
        let kind = Carton.Dec.kind v in
        let length = Carton.Dec.len v in
        let entry = Carton.Enc.make_entry ~kind ~length hash in
        fold (entry :: acc) rest
  in
  fold [] hashes >>| Array.of_list >>= fun entries ->
  Delta.delta
    ~threads:(List.init 4 (fun _ -> load store))
    ~weight:10 ~uid_ln:SHA256.length entries
  >>= fun targets -> Fiber.return (entries, targets)

let undeltify ~reporter store hashes =
  let open Fiber in
  let rec go targets = function
    | [] -> Fiber.return targets
    | hash :: hashes ->
        load store hash |> Scheduler.prj >>= fun v ->
        let kind = Carton.Dec.kind v in
        let length = Carton.Dec.len v in
        let entry = Carton.Enc.make_entry ~kind ~length hash in
        Carton.Enc.entry_to_target scheduler
          ~load:(fun _ -> Scheduler.inj (Fiber.return v))
          entry
        (* XXX(dinosaure): [load] is safe. *)
        |> Scheduler.prj
        >>= fun target ->
        reporter 0 >>= fun () -> go (target :: targets) hashes
  in
  go [] hashes >>= fun targets -> Fiber.return (Array.of_list targets)

let pack ~reporter ?level store targets stream =
  let header = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 12 in
  let offsets = Hashtbl.create (Array.length targets) in
  let crcs = Hashtbl.create (Array.length targets) in
  let find hash =
    match Hashtbl.find_opt offsets hash with
    | Some v -> Fiber.return (Some (Int64.to_int v))
    | None -> Fiber.return None
  in
  let uid =
    {
      Carton.Enc.uid_ln = SHA256.digest_size;
      Carton.Enc.uid_rw = SHA256.to_raw_string;
    }
  in
  let b =
    {
      Carton.Enc.o =
        Bigarray.Array1.create Bigarray.char Bigarray.c_layout De.io_buffer_size;
      Carton.Enc.i =
        Bigarray.Array1.create Bigarray.char Bigarray.c_layout De.io_buffer_size;
      Carton.Enc.q = De.Queue.create 0x1000;
      Carton.Enc.w = De.Lz77.make_window ~bits:15;
    }
  in
  let ctx = ref SHA256.empty in
  let cursor = ref 0L in
  Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12;
  stream (Some (Stdbob.bigstring_substring header ~off:0 ~len:12));
  ctx := SHA256.feed_bigstring !ctx header ~off:0 ~len:12;
  cursor := Int64.add !cursor 12L;
  let encode_target idx =
    let open Fiber in
    reporter () >>= fun () ->
    Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor;
    Carton.Enc.encode_target ?level scheduler ~b ~find:(Scheduler.inj <.> find)
      ~load:(load store) ~uid targets.(idx) ~cursor:(Int64.to_int !cursor)
    |> Scheduler.prj
    >>= fun (len, encoder) ->
    let payload = Stdbob.bigstring_substring b.o ~off:0 ~len in
    let crc = Crc32.digest_bigstring b.o 0 len Crc32.default in
    stream (Some payload);
    ctx := SHA256.feed_bigstring !ctx b.o ~off:0 ~len;
    cursor := Int64.add !cursor (Int64.of_int len);
    let rec go crc encoder =
      match Carton.Enc.N.encode ~o:b.o encoder with
      | `Flush (encoder, len) ->
          let payload = Stdbob.bigstring_substring b.o ~off:0 ~len in
          let crc = Crc32.digest_bigstring b.o 0 len crc in
          stream (Some payload);
          ctx := SHA256.feed_bigstring !ctx b.o ~off:0 ~len;
          cursor := Int64.add !cursor (Int64.of_int len);
          let encoder =
            Carton.Enc.N.dst encoder b.o 0 (Bigarray.Array1.dim b.o)
          in
          go crc encoder
      | `End ->
          Hashtbl.add crcs (Carton.Enc.target_uid targets.(idx)) crc;
          Fiber.return ()
    in
    go crc encoder
  in
  let encode_targets targets =
    let open Fiber in
    let rec go idx =
      if idx < Array.length targets then
        encode_target idx >>= fun () -> go (succ idx)
      else Fiber.return ()
    in
    go 0
  in
  let open Fiber in
  encode_targets targets >>= fun () ->
  let hash = SHA256.get !ctx in
  stream (Some (SHA256.to_raw_string hash));
  stream None;
  Fiber.return (hash, offsets, crcs)

let hash_of_filename filename =
  let tmp = Bytes.create 0x1000 in
  let ic = open_in_bin (Fpath.to_string filename) in
  let ln = in_channel_length ic in
  let rec go ctx =
    match input ic tmp 0 (Bytes.length tmp) with
    | 0 -> SHA256.get ctx
    | len -> go (SHA256.feed_bytes ctx tmp ~off:0 ~len)
    | exception End_of_file -> SHA256.get ctx
  in
  let ctx = SHA256.empty in
  let ctx = SHA256.feed_string ctx (Fmt.str "blob %d\000" ln) in
  let res = go ctx in
  close_in ic;
  res

let hash_of_directory rstore fpath =
  let entries = Filesystem.readdir fpath in
  let entries =
    List.filter_map
      (fun entry ->
        match Hashtbl.find_opt rstore Fpath.(fpath / entry) with
        | Some (hash, `Dir) -> Some (Fpath.(to_dir_path (v entry)), hash)
        | Some (hash, `Reg) -> Some (Fpath.v entry, hash)
        | None -> None)
      entries
  in
  let str = serialize_directory entries in
  let ctx = SHA256.empty in
  let ctx =
    SHA256.feed_string ctx (Fmt.str "tree %d\000" (String.length str))
  in
  let ctx = SHA256.feed_string ctx str in
  SHA256.get ctx

module Temp = Temp

type pattern = Temp.pattern

let store path =
  let rstore = Hashtbl.create 0x100 in
  let f path hashes =
    let stat = Unix.stat (Fpath.to_string path) in
    match stat.Unix.st_kind with
    | Unix.S_REG ->
        let hash = hash_of_filename path in
        Hashtbl.add rstore path (hash, `Reg);
        hash :: hashes
    | Unix.S_DIR ->
        let hash = hash_of_directory rstore path in
        Hashtbl.add rstore (Fpath.to_dir_path path) (hash, `Dir);
        hash :: hashes
    | _ -> hashes
  in
  let hashes = Filesystem.fold ~dotfiles:true ~f [] path in
  let store = Hashtbl.create (Hashtbl.length rstore) in
  Hashtbl.iter (fun v (k, _) -> Hashtbl.add store k v) rstore;
  (hashes, { store; rstore })

let deltify ~reporter store hashes =
  let open Fiber in
  deltify ~reporter store hashes >>= fun (_entries, targets) ->
  Fiber.return targets

let make ?(tmp : Temp.pattern = "pack-%s.pack") ?g ?level ~reporter store
    targets =
  let open Fiber in
  let tmp = Temp.random_temporary_path ?g tmp in
  let stream =
    let oc = open_out_bin (Fpath.to_string tmp) in
    function Some str -> output_string oc str | None -> close_out oc
  in
  pack ~reporter ?level store targets stream >>= fun (_hash, _offsets, _crcs) ->
  Fiber.return tmp

module First_pass = Carton.Dec.Fp (SHA256)
module Verify = Carton.Dec.Verify (SHA256) (Scheduler) (Fiber)

type status = Verify.status

let replace tbl k v =
  try
    let v' = Hashtbl.find tbl k in
    if v < v' then Hashtbl.replace tbl k v'
  with Not_found -> Hashtbl.add tbl k v

let digest ~kind ?(off = 0) ?len buf =
  let len =
    match len with Some len -> len | None -> Bigarray.Array1.dim buf - off
  in
  let ctx = SHA256.empty in
  let ctx =
    match kind with
    | `A -> SHA256.feed_string ctx (Fmt.str "root %d\000" len)
    | `B -> SHA256.feed_string ctx (Fmt.str "tree %d\000" len)
    | `C -> SHA256.feed_string ctx (Fmt.str "blob %d\000" len)
    | `D -> SHA256.feed_string ctx (Fmt.str "mesg %d\000" len)
  in
  let ctx = SHA256.feed_bigstring ctx ~off ~len buf in
  SHA256.get ctx

let first_pass ic ~reporter =
  let open Fiber in
  let oc = De.bigstring_create De.io_buffer_size in
  let zw = De.make_window ~bits:15 in
  let tp =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout De.io_buffer_size
  in
  let allocate _ = zw in
  First_pass.check_header scheduler
    (fun ic buf ~off ~len ->
      (Scheduler.inj <.> Fiber.return) (input ic buf off len))
    ic
  |> Scheduler.prj
  >>= fun (max, _, _) ->
  seek_in ic 0;
  let decoder = First_pass.decoder ~o:oc ~allocate `Manual in
  let children = Hashtbl.create 0x100 in
  let where = Hashtbl.create 0x100 in
  let weight = Hashtbl.create 0x100 in
  let length = Hashtbl.create 0x100 in
  let carbon = Hashtbl.create 0x100 in
  let crcs = Hashtbl.create 0x100 in
  let matrix = Array.make max Verify.unresolved_node in

  let rec go decoder =
    match First_pass.decode decoder with
    | `Await decoder ->
        let len = bigstring_input ic tp 0 (Bigarray.Array1.dim tp) in
        go (First_pass.src decoder tp 0 len)
    | `Peek decoder ->
        let keep = First_pass.src_rem decoder in
        let len = bigstring_input ic tp keep (Bigarray.Array1.dim tp - keep) in
        go (First_pass.src decoder tp 0 (keep + len))
    | `Entry
        ({ First_pass.kind = Base _; offset; size; consumed; crc; _ }, decoder)
      ->
        let n = First_pass.count decoder - 1 in
        Hashtbl.add crcs offset crc;
        Hashtbl.add weight offset size;
        Hashtbl.add length offset size;
        Hashtbl.add carbon offset consumed;
        Hashtbl.add where offset n;
        matrix.(n) <- Verify.unresolved_base ~cursor:offset;
        reporter 1 >>= fun () -> go decoder
    | `Entry
        ( {
            First_pass.kind = Ofs { sub = s; source; target };
            offset;
            size;
            consumed;
            crc;
            _;
          },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        let rel = Int64.(sub offset (of_int s)) in
        replace weight rel source;
        replace weight offset target;
        Hashtbl.add crcs offset crc;
        Hashtbl.add length offset size;
        Hashtbl.add carbon offset consumed;
        Hashtbl.add where offset n;
        (try
           let vs = Hashtbl.find children (`Ofs rel) in
           Hashtbl.replace children (`Ofs rel) (offset :: vs)
         with Not_found -> Hashtbl.add children (`Ofs rel) [ offset ]);
        reporter 1 >>= fun () -> go decoder
    | `Entry
        ( {
            First_pass.kind = Ref { ptr; target; source };
            offset;
            size;
            consumed;
            crc;
            _;
          },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        replace weight offset (Stdlib.max target source);
        Hashtbl.add crcs offset crc;
        Hashtbl.add length offset size;
        Hashtbl.add carbon offset consumed;
        Hashtbl.add where offset n;
        (try
           let vs = Hashtbl.find children (`Ref ptr) in
           Hashtbl.replace children (`Ref ptr) (offset :: vs)
         with Not_found -> Hashtbl.add children (`Ref ptr) [ offset ]);
        reporter 1 >>= fun () -> go decoder
    | `End hash ->
        let where ~cursor = Hashtbl.find where cursor in
        let children ~cursor ~uid =
          match
            ( Hashtbl.find_opt children (`Ofs cursor),
              Hashtbl.find_opt children (`Ref uid) )
          with
          | Some a, Some b -> List.sort_uniq compare (a @ b)
          | Some x, None | None, Some x -> x
          | None, None -> []
        in
        let weight ~cursor = Hashtbl.find weight cursor in
        let oracle = { Carton.Dec.where; children; digest; weight } in
        Fiber.return (Ok (hash, matrix, oracle, crcs))
    | `Malformed err -> Fiber.return (Error (`Msg err))
  in
  go decoder
