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
    let open Stream in
    let reporter _ = Fiber.return () and finalise = ignore in
    let filename = Bob_fpath.v "files/file.txt" in
    let* stream = Pack.make_one ~reporter ~finalise filename in
    let stream = Result.get_ok stream in
    let flow = Pack.analyse reporter in
    let buf0 = Buffer.create 0x7ff in
    let save bstr =
      Buffer.add_string buf0 (Bstr.to_string bstr);
      Fiber.return ()
    in
    let stream = Stream.via Flow.(flow >> tap save) stream in
    let* head = Stream.into Sink.first stream in
    match head with
    | None -> Alcotest.failf "Empty PACK stream"
    | Some (`Elt (off, _status, `Base (`D, _weight)), _decoder, _src, _off) ->
        let entry0 = Buffer.contents buf0 in
        Fmt.epr ">>> entry0: %d byte(s)\n%!" (String.length entry0);
        Fmt.epr ">>> @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.default) entry0;
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

let () = Alcotest.run "pack" [ ("simple", [ simple00 ]) ]
