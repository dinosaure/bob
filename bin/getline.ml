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

open Fiber

let stdin = Unix.descr_of_in_channel Stdlib.stdin
(* Windows compatibility. *)

let rec getline () =
  Fmt.pr ">>> %!";
  Fiber.getline stdin >>= function
  | Some line ->
      Fmt.pr "# %s\n%!" line;
      getline ()
  | None -> Fiber.return ()

let () =
  let rec go () =
    Fmt.pr ">>> %!";
    match input_line Stdlib.stdin with
    | line ->
        Fmt.pr "# %s\n%!" line;
        go ()
    | exception End_of_file -> ()
  in
  go ()
