type value = [ `Null | `Bool of bool | `String of string | `Float of float ]
type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]

let of_json ?(size_chunk = 0x1000) ic =
  let decoder = Jsonm.decoder `Manual in
  let buf = Bytes.create size_chunk in

  let await k `Await =
    match input ic buf 0 size_chunk with
    | len ->
        Jsonm.Manual.src decoder buf 0 len;
        k ()
    | exception End_of_file -> failwith "Partial input"
  in

  let error (`Error err) =
    Fmt.failwith "Invalid input: %a." Jsonm.pp_error err
  in
  let end_of_input `End = failwith "End of input" in

  (* arr := value* *)
  let rec arr acc k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> arr acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> core (fun v -> arr (v :: acc) k) v
  (* name := `Name n ':' value *)
  and name n k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> name n k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> core (fun v -> k (n, v)) v
  (* obj := name* *)
  and obj acc k =
    match Jsonm.decode decoder with
    | #await as v -> await (fun () -> obj acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v ->
        Fmt.failwith "Unexpected lexeme: %a (expected key)" Jsonm.pp_lexeme v
  (* t := value | obj | arr *)
  and core k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> failwith "Retrieve invalid end of array/object"
    | `Name _ -> failwith "Retrieve invalid key value"
  in

  (* top := t *)
  let rec top k () =
    match Jsonm.decode decoder with
    | #await as v -> await (top k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme (#Jsonm.lexeme as lexeme) -> core k lexeme
  in

  let res = ref None in
  top (fun value -> res := Some value) ();
  Option.get !res

let of_json ?size_chunk ic =
  try Ok (of_json ?size_chunk ic) with Failure msg -> Error (`Msg msg)

type t = [ value | `O of (string * t) list | `A of t list ]

module Stack = struct
  type stack =
    | In_array of t list * stack
    | In_object of (string * t) list * stack
    | Empty
end

let to_json ?minify ?(size_chunk = 0x1000) oc json =
  let encoder = Jsonm.encoder ?minify `Manual in
  let buf = Bytes.create size_chunk in
  let rec encode k stack value =
    match Jsonm.encode encoder value with
    | `Ok -> k stack
    | `Partial ->
        let len = Bytes.length buf - Jsonm.Manual.dst_rem encoder in
        output_string oc (Bytes.sub_string buf 0 len);
        Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
        encode k stack `Await
  and value k v stack =
    match v with
    | #value as v -> encode (continue k) stack (`Lexeme v)
    | `O ms -> encode (obj k ms) stack (`Lexeme `Os)
    | `A vs -> encode (arr k vs) stack (`Lexeme `As)
  and obj k ms stack =
    match ms with
    | (n, v) :: ms ->
        let stack = Stack.In_object (ms, stack) in
        encode (value k v) stack (`Lexeme (`Name n))
    | [] -> encode (continue k) stack (`Lexeme `Oe)
  and arr k vs stack =
    match vs with
    | v :: vs ->
        let stack = Stack.In_array (vs, stack) in
        value k v stack
    | [] -> encode (continue k) stack (`Lexeme `Ae)
  and continue k = function
    | Stack.In_array (vs, stack) -> arr k vs stack
    | Stack.In_object (ms, stack) -> obj k ms stack
    | Stack.Empty as stack -> encode k stack `End
  in

  Jsonm.Manual.dst encoder buf 0 (Bytes.length buf);
  value (Fun.const ()) json Stack.Empty
