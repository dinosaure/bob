open Fiber
open Stream

let ic_of_file = function
  | None -> Fiber.return Stream.stdin
  | Some path -> (
      Stream.of_file path >>= function
      | Ok stream -> Fiber.return stream
      | Error (`Msg err) -> Fmt.failwith "%s." err)

let compress ic oc =
  ic_of_file ic >>= fun ic ->
  let oc =
    Stdlib.Option.value ~default:Stream.stdout
      (Stdlib.Option.map Stream.to_file oc)
  in
  let flow =
    Flow.deflate_zlib ~q:(De.Queue.create 0x1000)
      ~w:(De.Lz77.make_window ~bits:15)
      ~level:4
  in
  Stream.via flow ic |> oc

let () = Fiber.run (compress None None)
