let src = Logs.Src.create "bob.pack"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA256 = struct
  include Digestif.SHA256

  let hash x = Hashtbl.hash x
  let length = digest_size
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

let load_file fpath =
  let fd = Unix.openfile (Fpath.to_string fpath) Unix.[ O_RDONLY ] 0o644 in
  let ln = Unix.lseek fd 0 Unix.SEEK_END in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let bf = Bytes.create ln in
  let _ = Unix.read fd bf 0 ln in
  Unix.close fd;
  let rs = Bigstringaf.create ln in
  Bigstringaf.blit_from_bytes bf ~src_off:0 rs ~dst_off:0 ~len:ln;
  Carton.Dec.v ~kind:`A rs

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
    (Bigstringaf.of_string str ~off:0 ~len:(String.length str))

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
      reporter !counter >>| fun () ->
      Fmt.pr "*%!";
      counter := 0
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

let pack store targets stream =
  let header = Bigstringaf.create 12 in
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
      Carton.Enc.o = Bigstringaf.create De.io_buffer_size;
      Carton.Enc.i = Bigstringaf.create De.io_buffer_size;
      Carton.Enc.q = De.Queue.create 0x1000;
      Carton.Enc.w = De.Lz77.make_window ~bits:15;
    }
  in
  let ctx = ref SHA256.empty in
  let cursor = ref 0L in
  Carton.Enc.header_of_pack ~length:(Array.length targets) header 0 12;
  stream (Some (Bigstringaf.substring header ~off:0 ~len:12));
  ctx := SHA256.feed_bigstring !ctx header ~off:0 ~len:12;
  cursor := Int64.add !cursor 12L;
  let encode_target idx =
    let open Fiber in
    Hashtbl.add offsets (Carton.Enc.target_uid targets.(idx)) !cursor;
    Carton.Enc.encode_target scheduler ~b ~find:(Scheduler.inj <.> find)
      ~load:(load store) ~uid targets.(idx) ~cursor:(Int64.to_int !cursor)
    |> Scheduler.prj
    >>= fun (len, encoder) ->
    let payload = Bigstringaf.substring b.o ~off:0 ~len in
    let crc = Crc32.digest_bigstring b.o 0 len Crc32.default in
    stream (Some payload);
    ctx := SHA256.feed_bigstring !ctx b.o ~off:0 ~len;
    cursor := Int64.add !cursor (Int64.of_int len);
    let rec go crc encoder =
      match Carton.Enc.N.encode ~o:b.o encoder with
      | `Flush (encoder, len) ->
          let payload = Bigstringaf.substring b.o ~off:0 ~len in
          let crc = Crc32.digest_bigstring b.o 0 len crc in
          stream (Some payload);
          ctx := SHA256.feed_bigstring !ctx b.o ~off:0 ~len;
          cursor := Int64.add !cursor (Int64.of_int len);
          let encoder =
            Carton.Enc.N.dst encoder b.o 0 (Bigstringaf.length b.o)
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

let fiber_ignore _ = Fiber.return ()

let make ?(tmp : Temp.pattern = "pack-%s.pack") ?g ?(reporter = fiber_ignore)
    store hashes =
  let open Fiber in
  let tmp = Temp.random_temporary_path ?g tmp in
  let stream =
    let oc = open_out_bin (Fpath.to_string tmp) in
    function Some str -> output_string oc str | None -> close_out oc
  in
  deltify ~reporter store hashes >>= fun (_entries, targets) ->
  pack store targets stream >>= fun (hash, _offsets, _crcs) ->
  Fmt.pr ">>> %a\n%!" Fpath.pp tmp;
  Fmt.pr ">>> %a\n%!" SHA256.pp hash;
  Fiber.return ()
