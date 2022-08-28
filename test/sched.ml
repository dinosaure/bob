let bob = ref "bob"

let launch ?ic ?oc ?ec args =
  let ic0, ic1 =
    match ic with
    | None ->
        let fd0, fd1 = Unix.pipe ~cloexec:true () in
        (fd0, `Fd fd1)
    | Some filename ->
        let ic = Unix.openfile filename Unix.[ O_RDONLY ] 0o644 in
        (ic, `Filename filename)
  in
  let oc0, oc1 =
    match oc with
    | None ->
        let fd0, fd1 = Unix.pipe ~cloexec:true () in
        (`Fd fd0, fd1)
    | Some filename ->
        let oc =
          Unix.openfile filename Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
        in
        (`Filename filename, oc)
  in
  let ec0, ec1 =
    match ec with
    | None ->
        let fd0, fd1 = Unix.pipe ~cloexec:true () in
        (`Fd fd0, fd1)
    | Some filename ->
        let ec =
          Unix.openfile filename Unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
        in
        (`Filename filename, ec)
  in
  let args = !bob :: args in
  let pid = Unix.create_process !bob (Array.of_list args) ic0 oc1 ec1 in
  Unix.close ic0;
  Unix.close oc1;
  Unix.close ec1;
  (pid, ic1, oc0, ec0)

type command =
  | Stop_itself of {
      args : string list;
      ic : string option;
      oc : string option;
      ec : string option;
    }
  | Stop_with_signal of {
      args : string list;
      signal : int;
      ic : string option;
      oc : string option;
      ec : string option;
    }

let pp_command ppf = function
  | Stop_itself { args; ic; oc; ec } ->
      Fmt.pf ppf "%a%a%a%a"
        Fmt.(list ~sep:(any " ") string)
        args
        Fmt.(option (const string " <" ++ string))
        ic
        Fmt.(option (const string " 1>" ++ string))
        oc
        Fmt.(option (const string " 2>" ++ string))
        ec
  | Stop_with_signal { args; signal; ic; oc; ec } ->
      Fmt.pf ppf "%a &%d%a%a%a"
        Fmt.(list ~sep:(any " ") string)
        args signal
        Fmt.(option (const string " <" ++ string))
        ic
        Fmt.(option (const string " 1>" ++ string))
        oc
        Fmt.(option (const string " 2>" ++ string))
        ec

let redirect_to_null fd =
  let null = Unix.openfile "/dev/null" Unix.[ O_WRONLY ] 0o644 in
  Unix.dup2 ~cloexec:true fd null

let redirect_to_null_if_fd = function
  | `Filename _ -> ()
  | `Fd fd -> ( try redirect_to_null fd with _exn -> ())

let close_if_fd = function
  | `Filename _ -> ()
  | `Fd fd -> ( try Unix.close fd with _exn -> ())

let reorder programs results =
  let tbl = Hashtbl.create (List.length results) in
  List.iter (fun (pid, status) -> Hashtbl.add tbl pid status) results;
  List.map
    (fun (pid, command) ->
      let status = Hashtbl.find tbl pid in
      (pid, command, status))
    programs

let run programs =
  let programs =
    List.map
      (function
        | Stop_itself { args; ic; oc; ec } as command ->
            let pid, ic, oc, ec = launch ?ic ?oc ?ec args in
            close_if_fd ic;
            redirect_to_null_if_fd oc;
            redirect_to_null_if_fd ec;
            (pid, command)
        | Stop_with_signal { args; ic; oc; ec; _ } as command ->
            let pid, ic, oc, ec = launch ?ic ?oc ?ec args in
            close_if_fd ic;
            redirect_to_null_if_fd oc;
            redirect_to_null_if_fd ec;
            (pid, command))
      programs
  in
  let to_wait, to_signal =
    List.partition
      (function
        | _pid, Stop_itself _ -> true | _pid, Stop_with_signal _ -> false)
      programs
  in
  let rec wait results = function
    | [] -> results
    | (pid, _command) :: rest ->
        let _pid', status = Unix.waitpid [] pid in
        wait ((pid, status) :: results) rest
  in
  let r0 = wait [] to_wait in
  let rec kill = function
    | [] -> ()
    | (_pid, Stop_itself _) :: rest -> kill rest
    | (pid, (Stop_with_signal { signal; _ } as _command)) :: rest ->
        Unix.kill pid signal;
        kill rest
  in
  kill to_signal;
  let r1 = wait [] to_signal in
  reorder programs (r0 @ r1)

let trim lst =
  let rec go acc = function
    | [] -> List.rev acc
    | "" :: rest -> go acc rest
    | x :: rest -> go (x :: acc) rest
  in
  go [] lst

let string_tl str =
  if String.length str = 0 then "" else String.sub str 1 (String.length str - 1)

let rec chop n str = if n <= 0 then str else chop (n - 1) (string_tl str)

let is_prefix prefix str =
  let rec go idx =
    if idx = String.length prefix then true
    else if prefix.[idx] = str.[idx] then go (succ idx)
    else false
  in
  if String.length prefix <= String.length str then go 0 else false

let signal_of_string str =
  match String.lowercase_ascii str with
  | "sigint" -> Sys.sigint
  | _ -> Fmt.invalid_arg "Invalid signal: %S" str

let signal_to_string n =
  if n = Sys.sigint then "SIGINT" else Fmt.to_to_string (Fmt.fmt "%02d") n

let parse_word str =
  if String.length str = 0 then `Noop
  else
    match str.[0] with
    | '<' -> `Stdin (Fpath.v (string_tl str))
    | '>' -> `Stdout (Fpath.v (string_tl str))
    | '&' -> `Signal (signal_of_string (string_tl str))
    | _ -> (
        match (is_prefix "1>" str, is_prefix "2>" str) with
        | true, _ -> `Stdout (Fpath.v (chop 2 str))
        | _, true -> `Stderr (Fpath.v (chop 2 str))
        | _ -> `Arg str)

let parse_line str =
  let line = String.split_on_char ' ' str in
  let line = trim line in
  let line = List.map (String.split_on_char '\t') line in
  let line = List.concat line in
  let line = List.map parse_word line in
  let args = ref [] in
  let ic = ref None in
  let oc = ref None in
  let ec = ref None in
  let signal = ref None in
  List.iter
    (function
      | `Arg arg -> args := arg :: !args
      | `Noop -> ()
      | `Stdin path -> ic := Some (Fpath.to_string path)
      | `Stdout path -> oc := Some (Fpath.to_string path)
      | `Stderr path -> ec := Some (Fpath.to_string path)
      | `Signal v -> signal := Some v)
    line;
  match !signal with
  | None -> Stop_itself { args = List.rev !args; ic = !ic; oc = !oc; ec = !ec }
  | Some signal ->
      Stop_with_signal
        { args = List.rev !args; signal; ic = !ic; oc = !oc; ec = !ec }

let pp_process_status ppf = function
  | Unix.WEXITED v -> Fmt.pf ppf "EXITED(%d)" v
  | Unix.WSIGNALED v -> Fmt.pf ppf "SIGNALED(%s)" (signal_to_string v)
  | Unix.WSTOPPED v -> Fmt.pf ppf "STOPPED(%d)" v

let print_result (_, command, status) =
  Fmt.pr "%s %a -> %a\n%!" !bob pp_command command pp_process_status status

let () =
  if Array.length Sys.argv > 1 then bob := Sys.argv.(1);
  let rec go programs =
    match input_line stdin with
    | str ->
        let command = parse_line str in
        go (command :: programs)
    | exception End_of_file -> List.rev programs
  in
  let results = run (go []) in
  List.iter print_result results
