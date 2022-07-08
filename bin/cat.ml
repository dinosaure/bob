open Fiber

let rec full_write fd str off len =
  Fiber.write Unix.stdout ~off ~len str >>= function
  | Error _err -> exit 1
  | Ok len' ->
      if len - len' > 0 then full_write fd str (off + len') (len - len')
      else Fiber.return ()

let rec cat () =
  Fiber.read Unix.stdin >>= function
  | Error _err -> exit 1
  | Ok `End -> Fiber.return ()
  | Ok (`Data bstr) ->
      full_write Unix.stdout bstr 0 (Bigarray.Array1.dim bstr) >>= cat

let () = Fiber.run (cat ())
