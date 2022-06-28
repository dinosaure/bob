type fd = Unix.file_descr
type error = Unix.error
type write_error = [ `Closed | `Unix of Unix.error ]

let pp_error ppf errno = Fmt.pf ppf "%s" (Unix.error_message errno)

let pp_write_error ppf = function
  | `Closed -> Fmt.pf ppf "Connection closed by peer"
  | `Unix errno -> Fmt.pf ppf "%s" (Unix.error_message errno)

let of_file_descr fd = Fiber.return (Ok fd)
let recv = Fiber.read
let send = Fiber.write
let close = Fiber.close
