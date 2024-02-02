type version = [ `Version of int ]
type t = [ version | `Resume | `Size ]

let to_string = function
  | `Version n -> Fmt.str "version=%d" n
  | `Resume -> "resume"
  | `Size -> "size"

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let int_of_string str =
  try Ok (int_of_string str) with _ -> error_msgf "Invalid number: %S" str

let of_string str =
  let ( >>| ) x f = Result.map f x in
  match Astring.String.cut ~sep:"=" str with
  | Some ("version", value) ->
      int_of_string value >>| fun value -> `Version value
  | Some _ -> error_msgf "Invalid capability: %S" str
  | None -> (
      match str with
      | "resume" -> Ok `Resume
      | "size" -> Ok `Size
      | _ -> error_msgf "Invalid capability: %S" str)

let common lst1 lst2 =
  let open Set.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
  end) in
  let set1 = of_list lst1 in
  let set2 = of_list lst2 in
  set1 |> inter set2 |> fun set -> fold (fun x acc -> x :: acc) set []

let has capabilities c = List.exists (( = ) c) capabilities

[@@@warning "-11"]

type 'a key = Version : version key

let get : type a. t list -> a key -> a option =
 fun capabilities -> function
  | Version -> (
      match
        List.find_opt (function `Version _ -> true | _ -> false) capabilities
      with
      | Some #version as value -> value
      | Some _ -> assert false
      | None -> None)
  | __else__ -> None
