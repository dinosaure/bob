let sockaddr_with_secure_port sockaddr secure_port =
  match sockaddr with
  | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, secure_port)
  | Unix.ADDR_UNIX _ -> invalid_arg "Invalid sockaddr"

let chat sockaddr ~identity ~ciphers ~shared_keys =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let open Fiber in
  Connect.connect socket sockaddr |> function
  | Error err ->
      Fmt.epr "%s: %a.\n%!" Sys.executable_name Connect.pp_error err;
      Fiber.return 1
  | Ok () -> (
      Bob_unix.init_peer socket ~identity >>= function
      | Error err ->
          Fmt.epr "%s: %a.\n%!" Sys.executable_name Bob_unix.pp_error err;
          Fiber.return 1
      | Ok () -> (
          let queue = Ke.Rke.create ~capacity:0x100 Bigarray.char in
          let flow = Bob_unix.Crypto.make ~ciphers ~shared_keys socket in
          let rec go lines recv =
            Fiber.npick
              [
                (fun () -> Fiber.wait recv >>| fun v -> `Recv v);
                (fun () -> Fiber.getline Unix.stdin >>| fun v -> `Send v);
              ]
            >>= function
            | `Send (Some line) -> (
                List.iter (Fmt.pr "<~ %s\n%!") lines;
                let send = Fmt.str "%s\r\n" line in
                Bob_unix.Crypto.send flow send ~off:0 ~len:(String.length send)
                >>= function
                | Ok _ ->
                    Fmt.pr "~> %!";
                    go [] recv
                | Error _ as err ->
                    Bob_unix.Crypto.close flow >>= fun () -> Fiber.return err)
            | `Recv (Some line) ->
                Fmt.pr "\n%!";
                List.iter (Fmt.pr "<~ %s\n%!") (line :: lines);
                Fmt.pr "~> %!";
                Fiber.fork (fun () -> Bob_unix.Crypto.getline queue flow)
                >>= go []
            | `Recv None | `Send None ->
                Bob_unix.Crypto.close flow >>= fun () -> Fiber.return (Ok ())
          in
          Fmt.pr "~> %!";
          Fiber.fork (fun () -> Bob_unix.Crypto.getline queue flow) >>= go []
          >>= function
          | Ok () -> Fiber.return 0
          | Error _err -> Fiber.return 1))
