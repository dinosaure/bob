let default =
  let from_env var ~absent =
    match Fpath.of_string (Sys.getenv var) with
    | Ok path
      when Sys.file_exists (Fpath.to_string path)
           && Sys.is_directory (Fpath.to_string path) ->
        path
    | _ | (exception Not_found) -> absent
  in
  if Sys.os_type = "Win32" then from_env "TEMP" ~absent:Fpath.(v "./")
  else from_env "TMPDIR" ~absent:Fpath.(v "/tmp")

let default = ref default
let set_default_directory path = default := path
let get_default_directory () = !default

type pattern = (string -> string, Format.formatter, unit, string) format4

let random_temporary_path ?g pattern =
  let g = match g with Some g -> g | None -> Random.State.make_self_init () in
  let rec try_new_path tries =
    let r = Random.State.bits g land 0xffffff in
    let p =
      Fpath.(get_default_directory () / Fmt.str pattern (Fmt.str "%06x" r))
    in
    if Sys.file_exists (Fpath.to_string p) then
      if tries > 0 then try_new_path (pred tries)
      else Fmt.failwith "Too many tries to create a non-existing path."
    else p
  in
  try_new_path 10
