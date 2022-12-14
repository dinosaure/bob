let connect inet_addr port =
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
  let errno = Unix.getsockopt_error socket in
  Unix.close socket;
  Option.is_none errno

let () =
  match Sys.argv with
  | [| _; "closed"; inet_addr; port |] ->
      while connect inet_addr port = true do
        Unix.sleepf 0.01
      done
  | [| _; "opened"; inet_addr; port |] ->
      while connect inet_addr port = false do
        Unix.sleepf 0.01
      done
  | _ -> Format.eprintf "%s [opened|closed] <addr> <port>\n%!" Sys.argv.(0)
