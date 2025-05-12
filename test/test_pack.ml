let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let split_at ?(off = 0) ~len:max str =
  let rec go acc (str, off, len) =
    if len <= 0 then List.rev acc
    else
      let max = Int.min len max in
      let sub = Bstr.string str ~off ~len:max in
      go (sub :: acc) (str, off + max, len - max)
  in
  go [] (str, off, String.length str - off)

let simple00 =
  Alcotest.test_case "simple file" `Quick @@ fun () ->
  let ( let* ) = Fiber.bind in
  let prgm () =
    let open Bob_stream in
    let filename = Bob_fpath.v "files/file.txt" in
    let reporter = Fiber.ignore and finalise = ignore in
    let* stream0 = Pack.make_one ~reporter ~finalise filename in
    let stream0 = Result.get_ok stream0 in
    let flow = Pack.analyse Fiber.ignore in
    let buf0 = Buffer.create 0x7ff in
    let save bstr =
      Buffer.add_string buf0 (Bstr.to_string bstr);
      Fiber.return ()
    in
    let stream1 = Stream.via Flow.(flow >> tap save) stream0 in
    let* head = Stream.into Sink.first stream1 in
    match head with
    | None -> Alcotest.failf "Empty PACK stream"
    | Some (`Elt (off, _, `Base (`D, _)), _, _, _) ->
        let entry0 = Buffer.contents buf0 in
        let from = Source.list (split_at ~off ~len:0x7ff entry0) in
        let* filename', _src =
          Stream.run ~from
            ~via:(Pack.inflate_entry ~reporter)
            ~into:Sink.to_string
        in
        Alcotest.(check string) "filename" filename' "file.txt";
        Fiber.return ()
    | _ -> Alcotest.failf "Unexpected PACK object"
  in
  Fiber.run (prgm ())

let simple01 =
  Alcotest.test_case "simple directory" `Quick @@ fun () ->
  let ( let* ) = Fiber.bind in
  let prgm () =
    let open Bob_stream in
    let directory = Bob_fpath.v "files/bar/" in
    let reporter = Fiber.ignore in
    let* uids, store = Pack.store directory in
    let stream0 = Pack.deltify ~reporter store uids in
    let flow0 = Flow.(Pack.make ~reporter store << bstr_to_string) in
    let stream1 = Stream.via flow0 stream0 in
    let temp = Bob_fpath.v (Filename.temp_file "pack-" "pack") in
    let* () = Stream.to_file temp stream1 in
    Fiber.return ()
  in
  Fiber.run (prgm ())

let () = Alcotest.run "pack" [ ("simple", [ simple00; simple01 ]) ]
