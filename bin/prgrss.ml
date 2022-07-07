let make_compression_progress ~total =
  let open Progress.Line in
  list [ spacer 32; count_to total; const "delta-ified object(s)" ]

let make_progress_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      count_to total;
      const "compressed object(s)";
    ]

let make_tranfer_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list [ brackets @@ bar ~style ~width:(`Fixed 30) total; percentage_of total ]

let incoming_data =
  let open Progress.Line in
  let spin =
    spinner
      ~frames:
        [
          "⠁";
          "⠉";
          "⠙";
          "⠚";
          "⠒";
          "⠂";
          "⠂";
          "⠒";
          "⠲";
          "⠴";
          "⠤";
          "⠄";
          "⠄";
          "⠤";
          "⠴";
          "⠲";
          "⠒";
          "⠂";
          "⠂";
          "⠒";
          "⠚";
          "⠙";
          "⠉";
          "⠁";
        ]
      ()
  in
  list [ spin; bytes; bytes_per_sec ]

let with_reporter ~config quiet t f =
  let reporter, finalise =
    match quiet with
    | true -> ((fun _ -> Fiber.return ()), ignore)
    | false ->
        let display = Progress.Multi.line t |> Progress.Display.start ~config in
        let[@warning "-8"] Progress.Reporter.[ reporter ] =
          Progress.Display.reporters display
        in
        ( (fun n ->
            reporter n;
            Progress.Display.tick display;
            Fiber.return ()),
          fun () -> Progress.Display.finalise display )
  in
  f (reporter, finalise)
