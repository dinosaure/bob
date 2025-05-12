open Stdbob

type tree =
  | Directory of { abs : Bob_fpath.t; filename : string; entries : tree list }
  | File of { abs : Bob_fpath.t; filename : string }

type root = Root of { root : Bob_fpath.t; entries : tree list }
type t = root list
type entry = [ `Dir of string | `File of string ]

type elt =
  | Filename of string
  | Folder of { dirname : string; entries : entry list }
  | Tree of { abs : Bob_fpath.t; entries : entry list }
  | Blob of Bob_fpath.t

let list_of_root filenames folders trees blobs = function
  | Root { entries = [ File { abs; filename } ]; _ } ->
      let blobs = Blob abs :: blobs in
      (Filename filename :: filenames, folders, trees, blobs)
  | Root { root; entries } ->
      let dirname = Bob_fpath.basename root in
      let to_entry = function
        | Directory { filename; _ } -> `Dir filename
        | File { filename; _ } -> `File filename
      in
      let rec fn (trees, blobs) entry = go trees blobs entry
      and go trees blobs = function
        | File { abs; _ } -> (trees, Blob abs :: blobs)
        | Directory { abs; entries; _ } ->
            let trees, blobs = List.fold_left fn (trees, blobs) entries in
            let entries = List.map to_entry entries in
            (Tree { abs; entries } :: trees, blobs)
      in
      let trees, blobs = List.fold_left fn (trees, blobs) entries in
      let entries = List.map to_entry entries in
      (filenames, Folder { dirname; entries } :: folders, trees, blobs)

let flatten roots =
  let fn (filenames, folders, trees, blobs) root =
    list_of_root filenames folders trees blobs root
  in
  let filenames, folders, trees, blobs =
    List.fold_left fn ([], [], [], []) roots
  in
  [] |> List.rev_append blobs |> List.append trees |> List.rev_append folders
  |> List.rev_append filenames

let exists ~filename:filename' entries =
  let fn = function
    | Directory { filename; _ } | File { filename; _ } ->
        String.equal filename filename'
  in
  List.exists fn entries

let to_branch abs directories edge =
  let rec go abs = function
    | [] | [ ""; _ ] | [ _ ] -> edge
    | filename :: segs ->
        let abs = Bob_fpath.add_seg abs filename in
        let abs = Bob_fpath.to_dir_path abs in
        Directory { abs; filename; entries = [ go abs segs ] }
  in
  go abs directories

let add tree root abs rel =
  let segs = Bob_fpath.segs rel in
  let edge =
    match List.rev segs with
    | "" :: filename :: _ -> Directory { abs; filename; entries = [] }
    | filename :: _ -> File { abs; filename }
    | [] -> assert false
  in
  let rec go cur entries = function
    | [] -> assert false
    | [ "" ] -> entries
    | [ ""; filename ] | [ filename ] ->
        if exists ~filename entries then entries else edge :: entries
    | seg :: segs ->
        let fn = function
          | File _ as value -> value
          | Directory { abs; filename; entries; _ } as value ->
              if String.equal filename seg then
                let entries = go abs entries segs in
                Directory { abs; filename; entries }
              else value
        in
        if exists ~filename:seg entries then List.map fn entries
        else to_branch cur (seg :: segs) edge :: entries
  in
  go root tree segs

let add roots abs =
  let fn (Root { root; entries } as value) =
    if Bob_fpath.equal abs root = false && Bob_fpath.is_prefix root abs then
      match Bob_fpath.relativize ~root abs with
      | Some rel ->
          let entries = add entries root abs rel in
          Root { root; entries }
      | None -> value
    else value
  in
  List.map fn roots

module Fs = struct
  let readdir =
    let readdir path =
      try Sys.readdir (Bob_fpath.to_string path) with _exn -> [||]
    in
    List.sort String.compare <.> Array.to_list <.> readdir

  let rec traverse ~get ~add visited stack ~fn acc =
    match stack with
    | [] -> acc
    | x :: stack ->
        if List.exists (Bob_fpath.equal x) visited then
          traverse ~get ~add visited stack ~fn acc
        else
          let contents = get x in
          let stack = add contents stack in
          let visited = x :: visited in
          let acc = traverse ~get ~add visited stack ~fn acc in
          fn x acc

  let fold ?(dotfiles = false) ~fn acc paths =
    let dir_child path acc filename =
      if (not dotfiles) && filename.[0] = '.' then acc
      else
        let path = Bob_fpath.add_seg path filename in
        if Sys.is_directory (Bob_fpath.to_string path) then
          Bob_fpath.to_dir_path path :: acc
        else path :: acc
    in
    let add stack vs = List.rev_append stack vs in
    let get path =
      let entries = readdir path in
      List.fold_left (dir_child path) [] entries
    in
    traverse ~get ~add [] paths ~fn acc

  let fold ?dotfiles ~fn acc path = fold ?dotfiles ~fn acc [ path ]
end

let make ~cwd targets =
  let fn target =
    if Bob_fpath.is_rel target then
      Bob_fpath.normalize Bob_fpath.(cwd // target)
    else target
  in
  let targets = List.map fn targets in
  match targets with
  | [ root ] when Bob_fpath.is_dir_path root ->
      let fn abs roots = add roots abs in
      let roots = [ Root { root; entries = [] } ] in
      Fs.fold ~fn roots root
  | [ filename ] ->
      let root, _ = Bob_fpath.split_base filename in
      let edge =
        File { abs = filename; filename = Bob_fpath.basename filename }
      in
      [ Root { root; entries = [ edge ] } ]
  | paths ->
      let incl path path' =
        Bob_fpath.equal path path' = false && Bob_fpath.is_prefix path' path
      in
      let fn path = if List.exists (incl path) paths then None else Some path in
      let paths = List.filter_map fn paths in
      let fn roots root =
        if Bob_fpath.is_dir_path root then
          let fn abs roots = add roots abs in
          let roots = Root { root; entries = [] } :: roots in
          Fs.fold ~fn roots root
        else
          let _, file = Bob_fpath.split_base root in
          let filename = Bob_fpath.basename file in
          let edge = File { abs = root; filename } in
          Root { root; entries = [ edge ] } :: roots
      in
      List.fold_left fn [] paths

let setup_paths strs =
  let exception Invalid_path of string in
  let fn str =
    if Sys.file_exists str then
      if Sys.is_directory str then Some Bob_fpath.(to_dir_path (v str))
      else Some (Bob_fpath.v str)
    else raise (Invalid_path str)
  in
  try `Ok (List.filter_map fn strs)
  with Invalid_path path ->
    let msg = Fmt.str "%s does not exist." path in
    `Error (true, msg)
