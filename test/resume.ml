let random_sha256 () =
  let res = Bytes.create Digestif.SHA256.digest_size in
  for i = 0 to Bytes.length res - 1 do
    Bytes.set res i (Char.unsafe_chr (Random.int 256))
  done;
  Digestif.SHA256.of_raw_string (Bytes.unsafe_to_string res)

let random_fpath () = Fpath.v "/tmp/foo"

open Bob_protocol

let resume = Alcotest.testable Resume.pp (fun a b -> Resume.compare a b = 0)

let isomorphism =
  Alcotest.test_case "isormophim" `Quick @@ fun () ->
  let t =
    let cur = (random_sha256 (), Int64.of_int (Random.bits ())) in
    let res = (random_sha256 (), Int64.of_int (Random.bits ())) in
    Resume.v1 ~cur ~res (random_fpath ())
  in
  let oc = open_out "resume.json" in
  Resume.to_json oc t;
  close_out oc;
  let ic = open_in "resume.json" in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  match Resume.of_json ic with
  | Ok t' -> Alcotest.(check resume) "isomorphism" t t'
  | Error (`Msg msg) -> Alcotest.failf "of_json(): %s" msg

let () = Alcotest.run "resume" [ ("v1", [ isomorphism ]) ]
