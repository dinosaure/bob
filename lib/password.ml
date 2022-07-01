let is_vowel = function 'a' | 'e' | 'i' | 'o' | 'u' -> true | _ -> false

let split word =
  let max = String.length word in
  let idx = ref 0 in
  while !idx < max && not (is_vowel word.[!idx]) do
    incr idx
  done;
  if !idx = max then None
  else (
    while !idx < max && is_vowel word.[!idx] do
      incr idx
    done;
    Some (String.sub word 0 !idx, String.sub word !idx (max - !idx)))

module Set = Set.Make (String)

let ( .!{} ) tbl k = Hashtbl.find tbl k
let ( .!{}<- ) tbl k v = Hashtbl.replace tbl k v

type key = Ends_up | Is of char | Begin_with of char

let insert_part_word ~wordparts chr part remains =
  if not (chr = '\000' && remains) then (
    let h =
      match (chr, remains) with
      | '\000', true -> assert false
      | '\000', false -> Ends_up
      | chr, true -> Is chr
      | chr, false -> Begin_with chr
    in
    if not (Hashtbl.mem wordparts h) then Hashtbl.add wordparts h Set.empty;
    wordparts.!{h} <- Set.add part wordparts.!{h})

type t = Set.t * (key, Set.t) Hashtbl.t

let compile dict : t =
  let wordparts = Hashtbl.create 0x100 in
  for c = 0 to Array.length dict - 1 do
    let word = ref dict.(c) in
    let chr = ref '\000' in
    while !word <> "" do
      match split !word with
      | Some (v, word') ->
          insert_part_word ~wordparts !chr v (word' = "");
          word := word';
          chr := v.[String.length v - 1]
      | None ->
          insert_part_word ~wordparts !chr !word true;
          word := ""
    done
  done;
  let starters = ref Set.empty in
  Hashtbl.filter_map_inplace
    (function
      | Is _ -> fun set -> Some set
      | Ends_up ->
          fun set ->
            starters := Set.union set !starters;
            None
      | Begin_with _ -> fun set -> Some set)
    wordparts;
  (!starters, wordparts)

let generate ?g (starters, wordparts) length =
  let g = match g with None -> Random.State.make_self_init () | Some g -> g in

  let buf = Buffer.create 0x100 in

  let starter =
    List.nth (Set.elements starters)
      (Random.State.int g (Set.cardinal starters))
  in
  let k = ref starter in
  Buffer.add_string buf !k;

  for h = 0 to length - 1 do
    let chr = !k.[String.length !k - 1] in
    let v = if h = length - 2 then Is chr else Begin_with chr in
    match Hashtbl.find_opt wordparts v with
    | Some words ->
        let words = Set.elements words in
        let word = List.nth words (Random.State.int g (List.length words)) in
        Buffer.add_string buf word;
        k := word
    | None -> ()
  done;
  Buffer.contents buf
