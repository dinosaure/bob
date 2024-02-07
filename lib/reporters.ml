open Stdbob

let compression_progress ~total =
  let open Progress.Line in
  list [ const ">>>"; spacer 32; count_to total; const "delta-ified object(s)" ]

let progress_bar_for_objects ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      const ">>>";
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      count_to total;
      const "compressed object(s)";
    ]

let progress_bar_for_file ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      const ">>>";
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      bytes;
      constf " / %a" (bytes_to_size ~decimals:2) total;
    ]

let verify_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      const ">>>";
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      count_to total;
      const "verified object(s)";
    ]

let extract_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      const ">>>";
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      count_to total;
      const "extracted object(s)";
    ]

let transfer_bar ~total =
  let open Progress.Line in
  let style = if Fmt.utf_8 Fmt.stdout then `UTF8 else `ASCII in
  list
    [
      const ">>>";
      brackets @@ bar ~style ~width:(`Fixed 30) total;
      percentage_of total;
    ]

let incoming_data =
  let open Progress.Line in
  let spin =
    spinner ~frames:[ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ] ()
  in
  list [ const ">>>"; spin; bytes; bytes_per_sec ]

let counter =
  let open Progress.Line in
  list [ const ">>>"; spacer 32; sum ~width:5 (); const "object(s)" ]

let with_reporter ~config quiet t f =
  let reporter, finalise =
    match quiet with
    | true -> (ignore, ignore)
    | false ->
        let display = Progress.Multi.line t |> Progress.Display.start ~config in
        let[@warning "-8"] Progress.Reporter.[ reporter ] =
          Progress.Display.reporters display
        in
        ( (fun n ->
            reporter n;
            Progress.Display.tick display),
          fun () -> Progress.Display.finalise display )
  in
  f (reporter, finalise)
