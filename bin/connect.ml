let rec connect fd sockaddr =
  try
    Unix.connect fd sockaddr;
    Ok ()
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> connect fd sockaddr
  | Unix.Unix_error (Unix.EACCES, _, _) -> (
      match sockaddr with
      | Unix.ADDR_UNIX _ -> Error `Permission_denied
      | Unix.ADDR_INET _ -> Error `Access_denied)
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Error `Local_address_already_in_use
  | Unix.Unix_error (Unix.EADDRNOTAVAIL, _, _) -> Error `Address_not_available
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> Error `Connection_refused
  | Unix.Unix_error (Unix.ENETUNREACH, _, _) -> Error `Network_unreachable
  | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> Error `Timeout
  | Unix.Unix_error (errno, f, arg) -> Error (`Unix (errno, f, arg))

type error =
  [ `Permission_denied
  | `Access_denied
  | `Local_address_already_in_use
  | `Address_not_available
  | `Connection_refused
  | `Network_unreachable
  | `Timeout
  | `Unix of Unix.error * string * string ]

let pp_error ppf = function
  | `Permission_denied -> Fmt.string ppf "Permission denied"
  | `Access_denied -> Fmt.string ppf "Access denied"
  | `Local_address_already_in_use ->
      Fmt.string ppf "Local address already in use"
  | `Address_not_available -> Fmt.string ppf "Address not available"
  | `Connection_refused -> Fmt.string ppf "Connection refused"
  | `Network_unreachable -> Fmt.string ppf "Network unreachable"
  | `Timeout -> Fmt.string ppf "Timeout"
  | `Unix (errno, f, arg) ->
      Fmt.pf ppf "%s(%s): %s" f arg (Unix.error_message errno)
