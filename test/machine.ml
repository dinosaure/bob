module SHA256 = Digestif.SHA256

let rec run_pipe a b =
  let open Bob_protocol.Protocol in
  match (a, b) with
  | ( Read { buffer = buf0; off = off0; len = len0; continue = k0 },
      Write { buffer = buf1; off = off1; len = len1; continue = k1 } ) ->
      let len = min len0 len1 in
      Fmt.epr "%d byte(s) [b] => [a]\n%!" len;
      Fmt.epr "@[<hov>%a@]\n%!"
        (Hxd_string.pp Hxd.default)
        (String.sub buf1 off1 len);
      Bytes.blit_string buf1 off1 buf0 off0 len;
      run_pipe (k0 (`Len len)) (k1 len)
  | ( Write { buffer = buf0; off = off0; len = len0; continue = k0 },
      Read { buffer = buf1; off = off1; len = len1; continue = k1 } ) ->
      let len = min len0 len1 in
      Fmt.epr "%d byte(s) [a] => [b]\n%!" len;
      Fmt.epr "@[<hov>%a@]\n%!"
        (Hxd_string.pp Hxd.default)
        (String.sub buf0 off0 len);
      Bytes.blit_string buf0 off0 buf1 off1 len;
      run_pipe (k0 len) (k1 (`Len len))
  | Return a, Return b -> Ok (a, b)
  | Error err, _ | _, Error err -> Error err
  | Read _, Read _ | Write _, Write _ ->
      failwith "Invalid machine (Read|Read or Write|Write)"
  | (Read _ | Write _), Return _ | Return _, (Read _ | Write _) ->
      failwith "Invalid machine"

let test_basic =
  Alcotest.test_case "basic" `Quick @@ fun () ->
  let open Bob_protocol in
  let ctx0 = Protocol.state () and ctx1 = Protocol.state () in
  let bob = Machine.bob ctx0 in
  let alice = Machine.alice ctx1 in
  match run_pipe bob alice with
  | Ok (`Ready_to_send, `Ready_to_recv None) ->
      Alcotest.(check pass) "result" () ()
  | Ok _ -> Alcotest.failf "Unexpected results from the state machine"
  | Error (#Protocol.error as err) ->
      Alcotest.failf "Got a protocol error: %a" Protocol.pp_error err
  | Error (#Machine.error as err) ->
      Alcotest.failf "Got a machine error: %a" Machine.pp_error err

let metadata =
  Alcotest.testable Bob_protocol.Metadata.pp Bob_protocol.Metadata.equal

let test_with_metadata =
  Alcotest.test_case "metadata" `Quick @@ fun () ->
  let open Bob_protocol in
  let ctx0 = Protocol.state () and ctx1 = Protocol.state () in
  let m = Metadata.v1 ~size:1L `Directory in
  let bob = Machine.bob ~metadata:m ctx0 in
  let alice = Machine.alice ctx1 in
  match run_pipe bob alice with
  | Ok (`Ready_to_send, `Ready_to_recv (Some m')) ->
      Alcotest.(check metadata) "metadata" m m'
  | Ok _ -> Alcotest.failf "Unexpected results from the state machine"
  | Error (#Protocol.error as err) ->
      Alcotest.failf "Got a protocol error: %a" Protocol.pp_error err
  | Error (#Machine.error as err) ->
      Alcotest.failf "Got a machine error: %a" Machine.pp_error err

let sha256 = Alcotest.testable SHA256.pp SHA256.equal

let test_with_resume =
  Alcotest.test_case "resume" `Quick @@ fun () ->
  let open Bob_protocol in
  let ctx0 = Protocol.state () and ctx1 = Protocol.state () in
  let r =
    Resume.v1
      ~cur:(SHA256.digest_string "foo", 0L)
      ~res:(SHA256.digest_string "bar", 1L)
      (Fpath.v "/")
  in
  let bob = Machine.bob ctx0 in
  let alice = Machine.alice ~resume:r ctx1 in
  match run_pipe bob alice with
  | Ok (`Ready_to_resume r', `Ready_to_recv None) ->
      Alcotest.(check (pair sha256 int64)) "resume" (Resume.current r) r'
  | Ok _ -> Alcotest.failf "Unexpected results from the state machine"
  | Error (#Protocol.error as err) ->
      Alcotest.failf "Got a protocol error: %a" Protocol.pp_error err
  | Error (#Machine.error as err) ->
      Alcotest.failf "Got a machine error: %a" Machine.pp_error err

let () =
  Alcotest.run "machine"
    [ ("v1", [ test_basic; test_with_metadata; test_with_resume ]) ]
