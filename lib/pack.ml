open Stdbob

let src = Logs.Src.create "bob.pack"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA256 = struct
  include Digestif.SHA256

  let hash x = Hashtbl.hash x
  let length = digest_size
  let feed = feed_bigstring
  let null = digest_string ""
  let compare a b = String.compare (to_raw_string a) (to_raw_string b)

  let sink_bigstring =
    Stream.Sink.make ~init:(Fiber.always empty)
      ~push:(fun ctx bstr -> Fiber.return (feed_bigstring ctx bstr))
      ~stop:(Fiber.return <.> get) ()

  let sink_string =
    Stream.Sink.make ~init:(Fiber.always empty)
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
}

let length { rstore; _ } = Hashtbl.length rstore

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

let load_file path =
  let len = (Unix.stat (Fpath.to_string path)).Unix.st_size in
  let fd = Unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644 in
  let ba = Unix.map_file fd Bigarray.char Bigarray.c_layout false [| len |] in
  let ba = Bigarray.array1_of_genarray ba in
  Unix.close fd;
  Fiber.return (Carton.Dec.v ~kind:`C ba)

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

let load : store -> SHA256.t -> (Carton.Dec.v, Scheduler.t) Carton.io =
 fun store hash ->
  match Hashtbl.find_opt store.store hash with
  | None ->
      Log.err (fun m -> m "The object %a does not exists." SHA256.pp hash);
      raise Not_found
  | Some path -> (
      let stat = Unix.stat (Fpath.to_string path) in
      match stat.Unix.st_kind with
      | Unix.S_REG -> Scheduler.inj (load_file path)
      | Unix.S_DIR -> Scheduler.inj (load_directory store.rstore path)
      | _ -> failwith "Invalid kind of object")

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

      Carton.Enc.encode_target ?level scheduler ~b:t.b
        ~find:(Scheduler.inj <.> find) ~load:(load store) ~uid:t.uid target
        ~cursor:(Int64.to_int t.cursor)
      |> Scheduler.prj
      >>= fun (len, encoder) ->
      k.push acc (Bigarray.Array1.sub t.b.o 0 len) >>= fun acc ->
      t.ctx <- SHA256.feed_bigstring t.ctx t.b.o ~off:0 ~len;
      t.cursor <- Int64.add t.cursor (Int64.of_int len);
      encode_target t ~push:k.push ~acc encoder >>= fun acc ->
      Fiber.return (t, acc)
    in
    let full (_, acc) = k.full acc in
    let stop (t, acc) =
      let open Fiber in
      let hash = SHA256.get t.ctx in
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
  let hdr = bigstring_of_string hdr ~off:0 ~len:(String.length hdr) in
  Stream.of_file path >>= function
  | Error (`Msg err) -> Fmt.failwith "%s." err
  | Ok stream ->
      let open Stream.Infix in
      Stream.(into SHA256.sink_bigstring (singleton hdr ++ stream))

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
  Stream.(into SHA256.sink_string (double hdr str))

let store path =
  let open Fiber in
  let rstore = Hashtbl.create 0x100 in
  let compute path =
    let stat = Unix.stat (Fpath.to_string path) in
    match stat.Unix.st_kind with
    | Unix.S_REG ->
        Log.debug (fun m -> m "Calculate the hash of %a" Fpath.pp path);
        hash_of_filename path >>= fun hash ->
        Hashtbl.add rstore path (hash, `Reg);
        Fiber.return (Some hash)
    | Unix.S_DIR ->
        Log.debug (fun m ->
            m "Calculate the hash of %a (directory)" Fpath.pp path);
        hash_of_directory rstore path >>= fun hash ->
        Hashtbl.add rstore (Fpath.to_dir_path path) (hash, `Dir);
        Fiber.return (Some hash)
    | _ -> Fiber.return None
  in
  let open Stream in
  let paths =
    Stream.of_iter @@ fun f ->
    Filesystem.fold ~dotfiles:false ~f:(fun path () -> f path) () path
  in
  let hashes = Stream.filter_map compute paths in
  (* XXX(dinosaure): populate [rstore] and [store]. *)
  Stream.to_list hashes >>= fun hashes ->
  let store = Hashtbl.create (Hashtbl.length rstore) in
  Hashtbl.iter (fun v (k, _) -> Hashtbl.add store k v) rstore;
  Log.debug (fun m ->
      m "The store contains %d objects." (Hashtbl.length rstore));
  Fiber.return (Stream.of_list hashes, { store; rstore })

let make ?level ~reporter store =
  let length = Hashtbl.length store.rstore in
  pack ~reporter ?level ~length store

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
  let oc = De.bigstring_create io_buffer_size in
  let zw = De.make_window ~bits:15 in
  let tp =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout io_buffer_size
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
    | `Entry ({ First_pass.kind = Base _; offset; size; _ }, decoder) ->
        let n = First_pass.count decoder - 1 in
        Hashtbl.add weight offset size;
        Hashtbl.add length offset size;
        Hashtbl.add where offset n;
        matrix.(n) <- Verify.unresolved_base ~cursor:offset;
        reporter 1 >>= fun () -> go decoder
    | `Entry
        ( { First_pass.kind = Ofs { sub = s; source; target }; offset; size; _ },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        let rel = Int64.(sub offset (of_int s)) in
        replace weight rel source;
        replace weight offset target;
        Hashtbl.add length offset size;
        Hashtbl.add where offset n;
        (try
           let vs = Hashtbl.find children (`Ofs rel) in
           Hashtbl.replace children (`Ofs rel) (offset :: vs)
         with Not_found -> Hashtbl.add children (`Ofs rel) [ offset ]);
        reporter 1 >>= fun () -> go decoder
    | `Entry
        ( { First_pass.kind = Ref { ptr; target; source }; offset; size; _ },
          decoder ) ->
        let n = First_pass.count decoder - 1 in
        replace weight offset (Stdlib.max target source);
        Hashtbl.add length offset size;
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
        Fiber.return (Ok (hash, matrix, oracle))
    | `Malformed err -> Fiber.return (Error (`Msg err))
  in
  go decoder
