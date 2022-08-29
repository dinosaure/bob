let run inet_addr port =
  let inet_addr = Unix.inet_addr_of_string inet_addr in
  let port = int_of_string port in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let proto = Unix.getprotobyname "tcp" in
  let socket = Unix.socket domain Unix.SOCK_STREAM proto.Unix.p_proto in
  Unix.set_nonblock socket;
  (try Unix.connect socket sockaddr with _exn -> ());
  let stop = ref false in
  while !stop do
    let _, _, res = Unix.select [] [] [ socket ] 0.01 in
    if res <> [] then stop := true
  done;
  let errno = Unix.getsockopt_int socket Unix.SO_ERROR in
  Unix.close socket;
  if errno = 0 then true else false

let () =
  match Sys.argv with
  | [| _; inet_addr; port |] ->
      while run inet_addr port do
        Unix.sleepf 0.01
      done
  | _ -> Format.eprintf "%s <addr> <port>\n%!" Sys.argv.(0)
