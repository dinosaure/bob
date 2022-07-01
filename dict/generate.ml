let split ic =
  let rec go acc =
    match input_line ic with
    | line ->
        let words = String.split_on_char ' ' line in
        let words = List.filter (( <> ) "") words in
        go (List.rev_append words acc)
    | exception End_of_file -> Array.of_list (List.rev acc)
  in
  go []

let ocamlize input output =
  let ic = open_in_bin input in
  let oc = open_out_bin output in
  let words = split ic in
  close_in ic;
  let ppf = Format.formatter_of_out_channel oc in
  Fmt.pf ppf "let words = @[<hov>%a@]%!" Fmt.(Dump.array (fmt "%S")) words

let () =
  match Sys.argv with
  | [| _; input; output |] when Sys.file_exists input -> ocamlize input output
  | _ -> Fmt.epr "%s <filename> <filename>" Sys.executable_name
