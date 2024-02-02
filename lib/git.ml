open Stdbob

let src = Logs.Src.create "bob.git"

module Log = (val Logs.src_log src : Logs.LOG)

module SHA1 = struct
  include Digestif.SHA1

  let length = digest_size

  let sink_bigstring ?(ctx = empty) () =
    Stream.Sink.make ~init:(Fiber.always ctx)
      ~push:(fun ctx bstr -> Fiber.return (feed_bigstring ctx bstr))
      ~stop:(Fiber.return <.> get) ()

  let sink_string ?(ctx = empty) () =
    Stream.Sink.make ~init:(Fiber.always ctx)
      ~push:(fun ctx str -> Fiber.return (feed_string ctx str))
      ~stop:(Fiber.return <.> get) ()
end

let v_space = Astring.String.Sub.of_string " "
let v_null = Astring.String.Sub.of_string "\x00"

let tree_of_string ?path str =
  let path_with ~name =
    match path with
    | Some path -> Bob_fpath.(path / name)
    | None -> Bob_fpath.v name
  in
  let one str =
    let open Astring.String.Sub in
    let ( >>= ) = Option.bind in
    cut ~sep:v_space str >>= fun (perm, str) ->
    cut ~sep:v_null str >>= fun (name, str) ->
    (try Some (with_range ~len:SHA1.length str) with _ -> None)
    >>= fun hash ->
    let str = with_range ~first:SHA1.length str in
    let hash = SHA1.of_raw_string (to_string hash) in
    match to_string perm with
    | "40000" -> Some (`Dir (path_with ~name:(to_string name), hash), str)
    | "100644" -> Some (`Reg (path_with ~name:(to_string name), hash), str)
    | _ -> failwith "Invalid kind of entry into a tree"
  in
  let rec go acc str =
    match one str with
    | Some (entry, str) -> go (entry :: acc) str
    | None -> List.rev acc
  in
  go [] (Astring.String.Sub.of_string str)

let v_space = Cstruct.string " "
let v_null = Cstruct.string "\x00"

type elt = [ `Reg of Bob_fpath.t * SHA1.t | `Dir of Bob_fpath.t * SHA1.t ]
type tree = elt list

let tree_of_cstruct ?path contents =
  let path_with ~name =
    match path with
    | Some path -> Bob_fpath.(path / name)
    | None -> Bob_fpath.v name
  in
  let init () = Fiber.return (Cstruct.of_bigarray contents) in
  let pull contents =
    let ( >>= ) = Option.bind in
    Cstruct.cut ~sep:v_space contents >>= fun (perm, contents) ->
    Cstruct.cut ~sep:v_null contents >>= fun (name, contents) ->
    (try Some (Cstruct.sub contents 0 SHA1.length) with _ -> None)
    >>= fun hash ->
    let contents = Cstruct.shift contents SHA1.length in
    let hash = SHA1.of_raw_string (Cstruct.to_string hash) in
    match Cstruct.to_string perm with
    | "40000" ->
        let path = path_with ~name:(Cstruct.to_string name) in
        Some (`Dir (path, hash), contents)
    | "100644" ->
        let path = path_with ~name:(Cstruct.to_string name) in
        Some (`Reg (path, hash), contents)
    | _ -> failwith "Invalid kind of entry into a tree"
  in
  let pull = Fiber.return <.> pull in
  let stop = Fiber.ignore in
  Stream.Source { init; pull; stop }

let digest ~kind ?(off = 0) ?len buf =
  let len =
    match len with Some len -> len | None -> Bigarray.Array1.dim buf - off
  in
  let ctx = SHA1.empty in
  let ctx =
    match kind with
    | `A -> SHA1.feed_string ctx (Fmt.str "commit %d\000" len)
    | `B -> SHA1.feed_string ctx (Fmt.str "tree %d\000" len)
    | `C -> SHA1.feed_string ctx (Fmt.str "blob %d\000" len)
    | `D -> SHA1.feed_string ctx (Fmt.str "mesg %d\000" len)
  in
  let ctx = SHA1.feed_bigstring ctx ~off ~len buf in
  SHA1.get ctx

let hash_of_root ~real_length ~root hash =
  let str =
    Fmt.str "%s\000%s%d" (Bob_fpath.basename root) (SHA1.to_raw_string hash)
      real_length
  in
  let hdr = Fmt.str "commit %d\000" (String.length str) in
  SHA1.digest_string (hdr ^ str)

module Filesystem = struct
  let readdir =
    let readdir d =
      try Sys.readdir (Bob_fpath.to_string d) with _exn -> [||]
    in
    Array.to_list <.> readdir
end

let serialize_directory entries =
  let entries =
    List.sort (fun (a, _) (b, _) -> Bob_fpath.compare a b) entries
  in
  let open Stream in
  let open Stream in
  Stream.of_list entries >>= fun (p, hash) ->
  match Bob_fpath.is_dir_path p with
  | true ->
      Stream.of_list
        [
          "40000 ";
          Bob_fpath.(to_string (rem_empty_seg p));
          "\x00";
          SHA1.to_raw_string hash;
        ]
      |> Fiber.return
  | false ->
      Stream.of_list
        [ "100644 "; Bob_fpath.to_string p; "\x00"; SHA1.to_raw_string hash ]
      |> Fiber.return

let hash_of_directory ~root:_ rstore path =
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
  let open Fiber in
  let open Stream in
  Stream.to_string (serialize_directory entries) >>= fun str ->
  Log.debug (fun m -> m "Serialization of %a:" Bob_fpath.pp path);
  Log.debug (fun m -> m "@[<hov>%a@]" (Hxd_string.pp Hxd.default) str);
  let hdr = Fmt.str "tree %d\000" (String.length str) in
  Stream.(into (SHA1.sink_string ()) (double hdr str))

let hash_of_filename path =
  let open Fiber in
  let open Stream in
  let len = Unix.(stat (Bob_fpath.to_string path)).Unix.st_size in
  let hdr = Fmt.str "blob %d\000" len in
  let ctx = SHA1.feed_string SHA1.empty hdr in
  Stream.of_file path >>= function
  | Error (`Msg err) -> Fmt.failwith "%s." err
  | Ok stream -> Stream.(into (SHA1.sink_bigstring ~ctx ()) stream)
