let kind tmp ~offset =
  let open Fiber in
  let open Stream in
  Stream.run ~from:(Source.file ~offset tmp)
    ~via:(Pack.inflate_entry ~reporter:Fiber.ignore)
    ~into:Sink.to_string >>= fun (root, source) ->
  Fiber.Option.iter Source.dispose source >>= fun () ->
  Fiber.return (Git.tree_of_string root) >>? fun elements ->
  match List.partition (function `Normal _ -> true | _ -> false) elements with
  | [ `Normal name ], [] -> Fiber.return (OK (`File name))
  | files, [] ->
    let names = List.map (function `Normal name -> name | _ -> assert false) files in
    Fiber.return (Ok (`Files names))
  | [], [ _directory ] -> Fiber.return (Ok `Directory)
  | _ -> Fiber.return (Error `Invalid_first_entry)

let extract ?g ?metadata from destination =
  let open Fiber in
  let open Stream in
  let tmp = Temp.random_temporary_path ?g "pack-%s.pack" in
  let via = Flow.(save_into tmp << Pack.analyse ignore) in
  Stream.run ~from ~via ~into:Sink.first >>= function
  | Some (`End _, _, _, _), _ | None, _ -> Fiber.return (Error `Empty_pack_file)
  | Some (`Elt (offset, _status, `Base (`B, _weight)), decoder, src, off), leftover ->
    begin kind tmp ~offset >>? fun kind -> match kind, Stdlib.Option.map Metadata.kind_of_document metadata with
    | `File name, (Some `File | None) -> assert false
    | `Files names, (Some `Files | None) -> assert false
    | `Directory, (Some `Directory | None) -> assert false
      collect_and_verify_with_reporter quiet ~config entry tmp decoder ~src ~off leftover
      >>= Pack.unpack tmp
      >>? fun (name, total, hash, pack) ->
    | _ -> Fiber.return (Error `Unexpected_kind_of_document) end
