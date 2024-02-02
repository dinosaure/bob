type t = V1 of { size : int64 option; kind : [ `Directory | `File | `Files ] }

let v1 ?size kind = V1 { size; kind }
let size_of_document = function V1 { size; _ } -> size
let kind_of_document = function V1 { kind; _ } -> kind

let pp_kind ppf = function
  | `File -> Fmt.string ppf "file"
  | `Files -> Fmt.string ppf "files"
  | `Directory -> Fmt.string ppf "directory"

let pp ppf = function
  | V1 { size; kind } ->
      Fmt.pf ppf "@[<1>(V1@ @[<hov>{ size= %a;@ kind= %a; }@])@]"
        Fmt.(Dump.option int64)
        size pp_kind kind

let equal a b =
  match (a, b) with
  | V1 { size = s0; kind = k0 }, V1 { size = s1; kind = k1 } ->
      s0 = s1 && k0 = k1
