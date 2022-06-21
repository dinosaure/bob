let () = Printexc.record_backtrace true

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stdout)
let () = Logs.set_level ~all:true (Some Logs.Debug)

let process_packet data =
  let ctx = Bob.Protocol.make () in
  let rec go data acc = function
    | Bob.Protocol.Done v -> go data (v :: acc) (Bob.Protocol.recv ctx)
    | Bob.Protocol.Fail err -> Error err
    | Rd { buf= dst; off= dst_off; len= dst_len; k; } ->
      ( match data with
      | `End | `Data (_, _, 0) -> Ok (List.rev acc)
      | `Data (str, off, len) ->
        let max = min dst_len len in
        Bytes.blit_string str off dst dst_off max ;
        go (`Data (str, off + max, len - max)) acc (k (`Len max)) )
    | Wr _ -> assert false in
  go data [] (Bob.Protocol.recv ctx)

let test01 =
  Alcotest.test_case "2 packets larger than internal buffer" `Quick @@ fun () ->
  let payload =
    [ "\x30\x30\x64\x33\x30\x38\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\x3a"   (* 00d308127.0.0.1: *)
    ; "\x34\x37\x39\x32\x32\x00\x30\x30\x64\x35\x30\x36\x99\xc9\x6c\x87"   (* 47922.00d506..l. *)
    ; "\x79\x45\x36\xd3\x28\x0d\x3d\x19\xeb\x73\xc8\x3d\x90\x30\xde\xac"   (* yE6.(.=..s.=.0.. *)
    ; "\xbe\x3b\x55\x6d\x10\xa8\xf9\x89\xe6\x29\xde\x57\xbc\x9e\x31\xba"   (* .;Um.....).W..1. *)
    ; "\x42\xe7\x15\x7b\x13\x31\xe6\x69\xeb\x43\x5c\xa8\xf8\xd1\x28\x4e"   (* B..{.1.i.C\...(N *)
    ; "\x9a\xe8\x47\xbb\x7d\x55\x8d\x2d\xfb\x42\xf6\xf4\x27\xf6\x09\x96"   (* ..G.}U.-.B..'... *)
    ; "\xae\xa5\x06\x4d\xf2\xd4\x17\xe4\xcb\x87\xbd\x06\x5a\x9d\xc3\xb4"   (* ...M........Z... *)
    ; "\x5e\xa0\x34\xc8\xaa\x7f\x64\x13\xcb\xfe\x35\x3e" ]                 (* ^.4...d...5>     *) in
  let data = let str = String.concat "" payload in `Data (str, 0, String.length str) in
  match process_packet data with
  | Ok [ (0x00d3, `Agreement "127.0.0.1:47922")
       ; 0x00d5, `Y_and_server_validator (_, _) ] -> ()
  | Ok _ -> Alcotest.fail "Unexpected packets"
  | Error err -> Alcotest.failf "%a" Bob.Protocol.pp_error err

let test02 =
  Alcotest.test_case "2 packets larger than internal buffer" `Quick @@ fun () ->
  let payload =
    [ "\x30\x30\x36\x63\x30\x37\xbd\x0b\xce\x54\xd0\x82\xf6\x18\xf5\x0a"   (* 006c07...T...... *)
    ; "\x63\x02\x9b\xab\xe9\x31\x77\xc9\x1b\xfa\xf1\x68\xe4\x29\x38\xc7"   (* c....1w....h.)8. *)
    ; "\xbd\xf3\xdf\x59\x9a\x72\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\x3a"   (* ...Y.r127.0.0.1: *)
    ; "\x34\x38\x31\x38\x32\x00\x30\x30\x37\x35\x30\x37\x2e\x79\xb6\xb3"   (* 48182.007507.y.. *)
    ; "\xf8\x7d\xe5\xb4\xef\xda\x6b\x7b\x67\x78\x78\x87\xf5\xfe\x35\x32"   (* .}....k{gxx...52 *)
    ; "\x0d\x0c\xd1\xfa\x84\x24\x5f\x9a\x00\xfe\x85\x4c\x31\x32\x37\x2e"   (* .....$_....L127. *)
    ; "\x30\x2e\x30\x2e\x31\x3a\x34\x38\x31\x38\x32\x00" ]                 (* 0.0.1:48182.     *) in
  let data = let str = String.concat "" payload in `Data (str, 0, String.length str) in
  match process_packet data with
  | Ok [ 0x006c, `X_and_client_identity (_, "127.0.0.1:48182")
       ; 0x0075, `X_and_client_identity (_, "127.0.0.1:48182") ] -> ()
  | Ok _ -> Alcotest.fail "Unexpected packets"
  | Error err -> Alcotest.failf "%a" Bob.Protocol.pp_error err

let test03 =
  Alcotest.test_case "2 packets larger than internal buffer" `Quick @@ fun () ->
  let payload =
    [ "\x30\x30\x65\x30\x30\x37\x89\x01\x70\xfa\x83\xb2\x2b\xb9\x1a\x3a"   (* 00e007..p...+..: *)
    ; "\x62\xd8\xaa\x3c\xcd\x8e\x9a\x28\x7c\x4f\xea\xe7\x08\xc7\xea\x41"   (* b..<...(|O.....A *)
    ; "\xb2\xc0\x32\xf4\x78\xdf\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\x3a"   (* ..2.x.127.0.0.1: *)
    ; "\x34\x39\x33\x30\x38\x00\x30\x30\x64\x65\x30\x35\x4e\x89\x18\xa6"   (* 49308.00de05N... *)
    ; "\x41\xde\x95\xb7\x5f\x79\x07\xfa\xbf\x3e\xa8\xfa\x3b\xf7\xfa\x57"   (* A..._y...>..;..W *)
    ; "\x12\x7a\x15\x50\xdc\x50\x75\xc9\x92\x3b\x54\x95\x30\x00\x2d\x51"   (* .z.P.Pu..;T.0.-Q *)
    ; "\x1b\xeb\x6e\x6c\xe9\x79\x39\xde\xf7\x14\x48\xc6\x07\x70\x89\x42"   (* ..nl.y9...H..p.B *)
    ; "\xb4\x3d\x10\xa5\xce\x9c\x2d\x65\x6e\xf5\x98\xb9" ]                 (* .=....-en...     *) in
  let data = let str = String.concat "" payload in `Data (str, 0, String.length str) in
  match process_packet data with
  | Ok [ 0x00e0, `X_and_client_identity (_, "127.0.0.1:49308")
       ; 0x00de, `Client_validator _ ] -> ()
  | Ok _ -> Alcotest.fail "Unexpected packets"
  | Error err -> Alcotest.failf "%a" Bob.Protocol.pp_error err

let () = Alcotest.run "protocol"
  [ "recv", [ test01; test02; test03 ] ]
