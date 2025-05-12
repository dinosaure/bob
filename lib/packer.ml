type t =
  | Directory of { abs : Bob_fpath.t; rel : Bob_fpath.t; entries : t list }
  | File of { abs : Bob_fpath.t; rel : Bob_fpath.t }

module Fs = struct
  let readdir =
    let readdir path =
      try Sys.readdir (Bob_fpath.to_string path) with _exn -> [||]
    in
    Array.to_list <.> readdir

  let rec traverse ~get ~add visited stack ~fn acc =
    match stack with
    | [] -> Fiber.return acc
    | x :: stack ->
        if List.exists (Bob_fpath.equal x) visited then
          traverse ~get ~add visited stack ~fn acc
        else
          let ( let* ) = Fiber.bind in
          let contents = get x in
          let stack = add contents stack in
          let visited = x :: visited in
          let* acc = traverse ~get ~add visited stack ~fn acc in
          fn x acc

  let fold ?(dotfiles = false) ~fn acc paths =
    let dir_child path acc filename =
      if (not dotfiles) && bname.[0] = '.' then acc
      else Bob_fpath.(path / filename) :: acc
    in
    let add stack vs = vs @ stack in
    let get path =
      let entries = readdir path in
      List.fold_left (dir_child path) [] entries
    in
    traverse ~get ~add [] paths ~fn acc

  let fold ?dotfiles ~fn acc path = fold ?dotfiles ~fn acc [ path ]
end

let make = function
  | [ path ] when Bob_fpath.is_dir_path path -> assert false
  | [ filename ] -> assert false
  | paths -> assert false
