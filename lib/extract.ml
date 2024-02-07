open Stdbob
open Reporters
open Bob_protocol

(* NOTE(dinosaure): the format of the incoming PACK file must be:
   +-----------------
   | PACK..........x.
   | = An inflated Tree object
   | |- An inflated Blob object (1 entry into our Tree object)
   | |- Or some objects which describe a "directory" (1 entry into our Tree
   |    object)
   | |- Or multiple Blob objects (multiple entries into our Tree object)
   +-----------------

   If we have something else, it's bad. The extractor deflates the first entry
   and, depending on entries into this Tree, we:
   1) extract one file
   2) extract multiple files
   3) extract one directory
*)

type error =
  [ `Empty_pack_file
  | `Invalid_first_entry
  | `Corrupted_pack_file
  | `Invalid_pack_file
  | `Unexpected_kind_of_document ]

let pp_error ppf = function
  | `Empty_pack_file -> Fmt.string ppf "Empty PACK file"
  | `Invalid_first_entry ->
      Fmt.string ppf "Invalid first entry into the incoming PACK file"
  | `Corrupted_pack_file -> Fmt.string ppf "Corrupted PACK file"
  | `Invalid_pack_file -> Fmt.string ppf "Invalid PACK file"
  | `Unexpected_kind_of_document -> Fmt.string ppf "Unexpected kind of document"

let kind tmp ~offset =
  let open Fiber in
  let open Stream in
  Stream.run ~from:(Source.file ~offset tmp)
    ~via:(Pack.inflate_entry ~reporter:Fiber.ignore)
    ~into:Sink.to_string
  >>= fun (root, source) ->
  Fiber.Option.iter Source.dispose source >>= fun () ->
  Fiber.return (Ok (Git.tree_of_string root)) >>? fun elements ->
  match List.partition (function `Reg _ -> true | _ -> false) elements with
  | [ `Reg name ], [] -> Fiber.return (Ok (`File name))
  | files, [] ->
      let names =
        List.map (function `Reg name -> name | _ -> assert false) files
      in
      Fiber.return (Ok (`Files names))
  | [], [ `Dir name ] -> Fiber.return (Ok (`Directory name))
  | _ -> Fiber.return (Error `Invalid_first_entry)

let print_received_file ~quiet src dst =
  if not quiet then
    if Bob_fpath.equal src dst then
      Fmt.pr ">>> Received a file: %a.\n%!" Bob_fpath.pp src
    else
      Fmt.pr ">>> Received a file: %a (save into %a).\n%!" Bob_fpath.pp src
        Bob_fpath.pp dst

let print_received_directory ~quiet src dst =
  if not quiet then
    if Bob_fpath.equal src dst then
      Fmt.pr ">>> Received a folder: %a.\n%!" Bob_fpath.pp src
    else
      Fmt.pr ">>> Received a folder: %a (save into %a).\n%!" Bob_fpath.pp src
        Bob_fpath.pp dst

let extract_file ~quiet ?g tmp decoder src off ~leftover ~name dst =
  let open Fiber in
  let open Stream in
  let from =
    match leftover with
    | Some leftover ->
        if Bigarray.Array1.dim src - off > 0 then
          Source.prepend Bigarray.Array1.(sub src off (dim src - off)) leftover
        else leftover
    | None -> Source.array [| src |]
  in
  let dst =
    if Sys.file_exists (Bob_fpath.to_string name) then (
      let dst = Temp.random_temporary_path ?g "bob-%s" in
      Logs.err (fun m ->
          m "%a already exists (we saved received file into: %a)" Bob_fpath.pp
            name Bob_fpath.pp dst);
      dst)
    else Stdlib.Option.value ~default:name dst
  in
  print_received_file ~quiet name dst;
  let ctx =
    match decoder with
    | None -> Digestif.SHA1.empty
    | Some decoder -> Pack.ctx decoder
  in
  let ctx = ref ctx in
  (* XXX(dinosaure): this is fragile but we are sure that we already feed
     the [ctx] with the first entry of the PACK file. The next [Stream.run]
     into our [kind] function will inflate the first entry and we don't need to
     re-calculate the [ctx] again. However, the next entry (the file) must
     complete the [ctx] (see [with_digest]). *)
  Stream.run ~from
    ~via:
      Flow.(
        with_digest (module Digestif.SHA1) ctx
        << save_into tmp
        << Pack.inflate_entry ~reporter:Fiber.ignore)
    ~into:(Sink.file dst)
  (* XXX(dinosaure): note that we save the stream into [tmp] BUT the file
     we don't ensure that [tmp] will be a well-formed PACK file. The only
     thing interesting into [tmp] is last bytes which permits to verify
     if we received correctly the PACK file. *)
  >>=
  fun ((), source) ->
  let hash = Digestif.SHA1.get !ctx in
  Fiber.Option.iter Source.dispose source >>= fun () ->
  (* TODO(dinosaure): remove [dst] if we get an error (like
     `Corrupted_pack_file). *)
  try
    let expected =
      let open Stdlib in
      let ic = open_in (Bob_fpath.to_string tmp) in
      let ln = in_channel_length ic in
      let tp = Bytes.create Digestif.SHA1.digest_size in
      if ln < Digestif.SHA1.digest_size then failwith "Corrupted file.";
      seek_in ic (ln - Digestif.SHA1.digest_size);
      really_input ic tp 0 Digestif.SHA1.digest_size;
      close_in ic;
      Digestif.SHA1.of_raw_string (Bytes.unsafe_to_string tp)
    in
    if Digestif.SHA1.equal hash expected then Fiber.return (Ok dst)
    else Fiber.return (Error `Corrupted_pack_file)
  with _exn -> Fiber.return (Error `Invalid_pack_file)

let collect ~quiet ~config first_entry tmp decoder src off ~leftover =
  let open Fiber in
  let open Stream in
  let is_base (_, status, _) = Pack.is_base status in
  let () = assert (is_base first_entry) in
  (* XXX(dinosaure): [collect] is used by [extract] and the latter calls our
     function **only if** it got a _base_ object. So, our assertion should
     always be [true]. *)
  let from =
    match leftover with
    | Some leftover ->
        if Bigarray.Array1.dim src - off > 0 then
          Source.prepend Bigarray.Array1.(sub src off (dim src - off)) leftover
        else leftover
    | None -> Source.array [||]
  in
  let offset = Bigarray.Array1.dim src - off in
  let offset = Int64.neg (Int64.of_int offset) in
  Logs.debug (fun m -> m "Start to re-analyzed the rest of the PACK file.");
  Stream.run ~from
    ~via:Flow.(save_into ~offset tmp << Pack.analyse ?decoder ignore)
    ~into:Sink.list
  >>= fun (entries, source) ->
  Logs.debug (fun m -> m "All objects are analyzed.");
  Fiber.Option.iter Source.dispose source >>= fun () ->
  let entries, _hash =
    let is_elt = function
      | `Elt _, _, _, _ -> true
      | `End _, _, _, _ -> false
    in
    List.partition is_elt entries
  in
  let entries =
    let only_elt = function
      | `Elt elt, _, _, _ -> elt
      | `End _, _, _, _ -> assert false
    in
    List.map only_elt entries
  in
  let total =
    List.fold_left
      (fun count -> function
        | _, _, (`Ofs _ | `Ref _) -> succ count
        | _, _, `Base _ -> count)
      0 entries
  in
  let entries = Source.list (first_entry :: entries) in
  Pack.collect entries >>= fun (status, oracle) ->
  Logs.debug (fun m -> m "All objects are collected.");
  Reporters.with_reporter ~config quiet (verify_bar ~total)
  @@ fun (reporter, finalise) ->
  Pack.verify ~reporter:(reporter <.> Stdbob.always 1) ~oracle tmp status
  >>= fun () ->
  Logs.debug (fun m -> m "All objects are verified.");
  finalise ();
  Fiber.return status

let extract_directory ~quiet ?g ~config ~total pack ~name dst hash =
  let dst =
    if Sys.file_exists (Bob_fpath.to_string name) then (
      let dst = Temp.random_temporary_path ?g "bob-%s" in
      Logs.err (fun m ->
          m "%a already exists (we saved received file into: %a)" Bob_fpath.pp
            name Bob_fpath.pp dst);
      dst)
    else Stdlib.Option.value ~default:name dst
  in
  let open Fiber in
  print_received_directory ~quiet name dst;
  Reporters.with_reporter ~config quiet (extract_bar ~total:(pred total))
  @@ fun (reporter, finalise) ->
  reporter 1;
  (* XXX(dinosaure): report the root directory. *)
  (* TODO(dinosaure): at this stage, the only error which can error is an I/O
     error (no space left or something like that). We should catch this kind of
     errors and clean-up what we did (delete the root directory). *)
  Pack.create_directory ~reporter pack dst hash >>= fun _pack ->
  finalise ();
  Fiber.return (Ok dst)

let extract ~quiet ?g ?metadata ~config from dst =
  let open Fiber in
  let open Stream in
  let tmp = Temp.random_temporary_path ?g "pack-%s.pack" in
  let via = Flow.(save_into tmp << Pack.analyse ignore) in
  Stream.run ~from ~via ~into:Sink.first >>= function
  | Some (`End _, _, _, _), _ | None, _ -> Fiber.return (Error `Empty_pack_file)
  | ( Some
        ( `Elt ((offset, _status, `Base (`B, _weight)) as entry),
          decoder,
          src,
          off ),
      leftover ) -> (
      kind tmp ~offset >>? fun kind ->
      match (kind, Stdlib.Option.map Metadata.kind_of_document metadata) with
      | `File (name, _hash), (Some `File | None) ->
          extract_file ~quiet ?g tmp decoder src off ~leftover ~name dst
      | `Files _names, (Some `Files | None) -> assert false
      | `Directory (name, hash), (Some `Directory | None) ->
          collect ~quiet ~config entry tmp decoder src off ~leftover
          >>= fun status ->
          Pack.pack tmp status >>= fun pack ->
          let total = Array.length status in
          extract_directory ~quiet ~config ~total pack ~name dst hash
      | _ -> Fiber.return (Error `Unexpected_kind_of_document))
  | _ -> Fiber.return (Error `Invalid_first_entry)
