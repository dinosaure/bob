open Fiber
open Stream

let ic_of_file = function
  | None -> assert false (* TODO *)
  | Some path -> begin
      let bstr = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0x7ff in
      let fn str =
        let src_off = 0 and dst_off = 0 and len = String.length str in
        Stdbob.bigstring_blit_from_string str ~src_off bstr ~dst_off ~len;
        bstr
      in
      Stream.of_file ~len:0x7ff ~fn path >>= function
      | Ok stream -> Fiber.return stream
      | Error (`Msg err) -> Fmt.failwith "%s." err
    end

let compress ic oc =
  ic_of_file ic >>= fun ic ->
  let oc =
    let open Stdlib.Option in
    value ~default:Stream.stdout (map Stream.to_file oc)
  in
  let flow =
    let open Flow in
    let q = De.Queue.create 0x1000 in
    let w = De.Lz77.make_window ~bits:15 in
    deflate_zlib ~q ~w 4 << bigstring_to_string
  in
  Stream.via flow ic |> oc

let () = Fiber.run (compress None None)
