let src = Logs.Src.create "bob.unix"

module Log = (val Logs.src_log src : Logs.LOG)

module type IO = sig
  type fd
  type error
  type write_error = private [> `Closed ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
  val of_file_descr : Unix.file_descr -> (fd, write_error) result Fiber.t
  val recv : fd -> ([ `End | `Data of string ], error) result Fiber.t

  val send :
    fd -> off:int -> len:int -> string -> (int, write_error) result Fiber.t

  val close : fd -> unit Fiber.t
end

module Fiber = Fiber
open Fiber

let map_read = function
  | Ok (`Data str) -> `Read (`Data (str, 0, String.length str))
  | Ok `End -> `Read `End
  | Error err -> `Error (`Rd err)

let pp_data ppf = function
  | `End -> Fmt.pf ppf "<end>"
  | `Data (str, off, len) ->
      Fmt.pf ppf "@[<hov>%a@]"
        (Hxd_string.pp Hxd.default)
        (String.sub str off len)

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let serve_when_ready ?stop ~handler socket =
  let stop =
    match stop with Some stop -> stop | None -> Fiber.Ivar.create ()
  in
  let rec loop () =
    Fiber.pick
      (fun () -> Fiber.wait stop >>| fun () -> `Stop)
      (fun () -> Fiber.accept socket >>| fun v -> `Accept v)
    >>= function
    | `Stop -> Fiber.return ()
    | `Accept (fd, sockaddr) ->
        Log.debug (fun m ->
            m "Got a new connection from %a." pp_sockaddr sockaddr);
        let _ = Fiber.async (fun () -> handler fd sockaddr) in
        Fiber.pause () >>= loop
  in
  loop ()

module Make (IO : IO) = struct
  let rec full_write fd str ~off ~len =
    IO.send fd str ~off ~len >>= function
    | Error _ as err -> Fiber.return err
    | Ok len' ->
        if len - len' > 0 then
          full_write fd str ~off:(off + len') ~len:(len - len')
        else Fiber.return (Ok ())

  type error =
    [ `Connection_closed_by_relay
    | `Wr of IO.write_error
    | `Rd of IO.error
    | Bob.Protocol.error ]

  let pp_error ppf = function
    | `Connection_closed_by_relay -> Fmt.string ppf "Connection closed by relay"
    | `Wr `Closed -> Fmt.pf ppf "Connection closed"
    | `Wr err -> Fmt.pf ppf "send(): %a" IO.pp_write_error err
    | `Rd err -> Fmt.pf ppf "recv(): %a" IO.pp_error err
    | #Bob.Protocol.error as err -> Bob.Protocol.pp_error ppf err

  type income =
    [ `Read of [ `End | `Data of string * int * int ] | `Error of error ]

  type outcome = [ `Write of string | `Error of error | `Done | `Continue ]

  let run ~choose ~agreement ~receive ~send socket t =
    let handshake_is_done : [ `Done ] Fiber.Ivar.t = Fiber.Ivar.create () in
    let errored : [ `Error of error ] Fiber.Ivar.t = Fiber.Ivar.create () in
    let rec read () =
      Fiber.npick
        [
          (fun () -> IO.recv socket >>| map_read);
          (fun () -> (Fiber.wait errored :> income Fiber.t));
        ]
      >>= function
      | `Error err ->
          Log.err (fun m -> m "Got a global error: %a" pp_error err);
          Fiber.return (Error err)
      | `Read data -> (
          Log.debug (fun m -> m "recv <- %a" pp_data data);
          match receive t data with
          | `Continue | `Read -> Fiber.pause () >>= read
          | `Agreement identity ->
              choose identity >>= fun v ->
              agreement t v;
              Fiber.pause () >>= read
          | `Done shared_keys ->
              Fiber.Ivar.fill handshake_is_done `Done;
              Log.debug (fun m -> m "Finish the read fiber.");
              IO.close socket >>= fun () -> Fiber.return (Ok shared_keys)
          | `Close ->
              Log.err (fun m -> m "The relay closed the connection.");
              Fiber.Ivar.fill errored (`Error `Connection_closed_by_relay);
              Fiber.return (Error `Connection_closed_by_relay)
          | `Error (#Bob.Protocol.error as err) ->
              Log.err (fun m -> m "Got a recv error: %a" pp_error err);
              Fiber.Ivar.fill errored (`Error (err :> error));
              Fiber.return (Error err))
    in
    let rec write () =
      let send () = Fiber.return (send t) in
      Fiber.npick
        [
          (fun () -> (Fiber.wait handshake_is_done :> outcome Fiber.t));
          (fun () -> (Fiber.wait errored :> outcome Fiber.t));
          (fun () -> (send () :> outcome Fiber.t));
        ]
      >>= function
      | `Done ->
          Log.debug (fun m -> m "Finish the write fiber.");
          Fiber.return (Ok ())
      | `Continue -> Fiber.pause () >>= write
      | `Error err ->
          Log.err (fun m -> m "Got an error: %a" pp_error err);
          if Fiber.Ivar.is_empty errored then
            Fiber.Ivar.fill errored (`Error err);
          Fiber.return (Error err)
      | `Write str -> (
          Log.debug (fun m ->
              m "send -> @[<hov>%a@]" (Hxd_string.pp Hxd.default) str);
          full_write socket str ~off:0 ~len:(String.length str) >>= function
          | Ok () -> Fiber.pause () >>= write
          | Error `Closed ->
              (* XXX(dinosaure): according to our protocol, only the relay is able
                 to close the connection. *)
              if not (Fiber.Ivar.is_empty handshake_is_done) then
                Fiber.return (Ok ())
              else (
                if Fiber.Ivar.is_empty errored then
                  Fiber.Ivar.fill errored (`Error (`Wr `Closed));
                Fiber.return (Error (`Wr `Closed)))
          | Error err ->
              Log.err (fun m -> m "Got a write error: %a" IO.pp_write_error err);
              if Fiber.Ivar.is_empty errored then
                Fiber.Ivar.fill errored (`Error (`Wr err));
              Fiber.return (Error (`Wr err)))
    in
    Fiber.fork_and_join write read >>= function
    | Ok (), Ok shared_keys ->
        Log.debug (fun m -> m "The peer finished correctly.");
        Fiber.return (Ok shared_keys)
    (* XXX(dinosaure): only the [`Done] case in the loop is able
       to close the socket. For errors, we must properly close
       the connection. *)
    | Error err, _ -> IO.close socket >>= fun () -> Fiber.return (Error err)
    | _, Error err -> IO.close socket >>= fun () -> Fiber.return (Error err)

  let server socket ~g ~secret =
    let t = Bob.Server.hello ~g ~secret in
    let choose _ = assert false in
    let agreement _ = assert false in
    run ~choose ~agreement ~receive:Bob.Server.receive ~send:Bob.Server.send
      socket t

  let client socket ~choose ~g ~password =
    let identity = Unix.gethostname () in
    let t = Bob.Client.make ~g ~password ~identity in
    run ~choose ~agreement:Bob.Client.agreement ~receive:Bob.Client.receive
      ~send:Bob.Client.send socket t

  let relay ?(timeout = 3600.) socket rooms ~stop =
    let t = Bob.Relay.make () in
    let fds = Hashtbl.create 0x100 in
    let rec write () =
      let send_to () = Fiber.return (Bob.Relay.send_to t) in
      Fiber.npick [ (fun () -> Fiber.wait stop >>| fun () -> `Stop); send_to ]
      >>= function
      | `Stop -> Fiber.return ()
      | `Continue -> Fiber.pause () >>= write
      | `Close identity ->
          Log.debug (fun m -> m "Close %s" identity);
          (match Hashtbl.find_opt fds identity with
          | Some (fd, _) ->
              Hashtbl.remove fds identity;
              IO.close fd
          | None -> Fiber.return ())
          >>= write
      | `Write (identity, str) ->
          (match Hashtbl.find_opt fds identity with
          | None ->
              Log.err (fun m ->
                  m "%s does not exists as an active peer." identity);
              Bob.Relay.rem_peer t ~identity;
              write ()
          | Some (fd, _) -> (
              Log.debug (fun m ->
                  m "to   [%20s] <- @[<hov>%a@]" identity
                    (Hxd_string.pp Hxd.default)
                    str);
              full_write fd str ~off:0 ~len:(String.length str) >>= function
              | Ok v -> Fiber.return v
              | Error _err ->
                  Hashtbl.remove fds identity;
                  IO.close fd))
          >>= write
    in
    let rec read () =
      Fiber.npick
        [
          (fun () -> Fiber.wait stop >>| fun () -> `Stop);
          (fun () -> Fiber.return `Continue);
        ]
      >>= function
      | `Stop -> Fiber.return ()
      | `Continue ->
          Hashtbl.filter_map_inplace
            (fun identity (fd, ivar) ->
              match Fiber.Ivar.get ivar with
              | None -> Some (fd, ivar)
              | Some `Delete ->
                  Log.debug (fun m -> m "Delete %s from the reader" identity);
                  None
              | Some `Continue ->
                  let ivar = Fiber.Ivar.create () in
                  let thread () =
                    match Hashtbl.mem fds identity with
                    | false -> Fiber.return `Delete
                    | true -> (
                        IO.recv fd >>| map_read >>= function
                        | `Error (`Rd err) ->
                            Log.err (fun m ->
                                m "Got an error from %s: %a" identity
                                  IO.pp_error err);
                            Fiber.return `Delete
                        | `Read `End ->
                            Log.debug (fun m ->
                                m "Got end of input from %s" identity);
                            Fiber.return `Delete
                        | `Read (`Data _ as data) -> (
                            Log.debug (fun m ->
                                m "from [%20s] -> %a" identity pp_data data);
                            match Bob.Relay.receive_from t ~identity data with
                            | `Close -> Fiber.return `Delete
                            | `Agreement (peer0, peer1) ->
                                Bob.Secured.add_peers rooms peer0 peer1;
                                Fiber.return `Continue
                            | `Read | `Continue -> Fiber.return `Continue))
                  in
                  Fiber.async (fun () -> thread () >>| Fiber.Ivar.fill ivar);
                  Some (fd, ivar))
            fds;
          Fiber.pause () >>= read
    in
    let handler fd sockaddr =
      let identity = Fmt.str "%a" pp_sockaddr sockaddr in
      Log.debug (fun m -> m "Add %s as an active connection." identity);
      IO.of_file_descr fd >>= function
      | Error err ->
          Log.err (fun m ->
              m
                "Error when we upgraded the incoming file-description to our \
                 flow: %a"
                IO.pp_write_error err);
          Fiber.close fd
      | Ok fd ->
          Hashtbl.add fds identity (fd, Fiber.Ivar.full `Continue);
          Bob.Relay.new_peer t ~identity;

          (* XXX(dinosaure): handle timeout. *)
          Fiber.async (fun () ->
              Fiber.sleep timeout >>= fun () ->
              if Bob.Relay.exists t ~identity then (
                Log.warn (fun m -> m "%s timeout" identity);
                Bob.Relay.rem_peer t ~identity);
              Fiber.return ());

          Fiber.return ()
    in
    fork_and_join
      (fun () -> serve_when_ready ~stop ~handler socket)
      (fun () -> fork_and_join read write >>= fun ((), ()) -> Fiber.return ())
    >>= fun ((), ()) -> Fiber.return ()
end

module Crypto = Bob.Crypto

let rec full_write fd str ~off ~len =
  Fiber.write fd str ~off ~len >>= function
  | Error _ as err -> Fiber.return err
  | Ok len' ->
      if len - len' > 0 then
        full_write fd str ~off:(off + len') ~len:(len - len')
      else Fiber.return (Ok ())

type outcome =
  [ `Closed
  | `Bounded of string * string
  | `Error of [ `Rd of Unix.error ]
  | `Continue of Bob.Secured.reader
  | `Invalid_peer
  | `Peer of string * string ]

module Psq =
  Psq.Make
    (String)
    (struct
      type t = [ `Bound of string | `Ready ]

      let compare a b =
        match (a, b) with
        | `Bound _, `Ready -> -1
        | `Ready, `Bound _ -> 1
        | `Bound _, `Bound _ | `Ready, `Ready -> 0
    end)

let rec loop ~timeout t ready queue state fd k =
  Fiber.npick
    [
      (fun () -> (Fiber.wait state :> outcome Fiber.t));
      (fun () ->
        Fiber.read fd >>| map_read >>| function
        | `Error _ as err -> err
        | `Read data -> (k data :> outcome));
    ]
  >>= function
  | `Closed | `Bounded _ -> Fiber.return ()
  | `Continue k -> loop t ~timeout ready queue state fd k
  | `Error _ | `Invalid_peer ->
      Log.err (fun m ->
          m "Got an error from %a." pp_sockaddr (Unix.getpeername fd));
      if Fiber.Ivar.is_empty state then (
        Fiber.Ivar.fill state `Closed;
        full_write fd "02" ~off:0 ~len:2 >>= fun _ -> Fiber.close fd)
      else Fiber.return ()
  | `Peer (peer0, peer1) ->
      Log.debug (fun m -> m "Allocate a secure room for %s and %s." peer0 peer1);
      if Psq.mem peer1 !queue then (
        queue := Psq.remove peer1 !queue;
        queue :=
          Psq.add peer0 (`Bound peer1) !queue |> Psq.add peer1 (`Bound peer0))
      else queue := Psq.add peer0 `Ready !queue;
      if Fiber.Ivar.is_empty state then (
        Fiber.Ivar.fill state (`Bounded (peer0, peer1));
        let state : [ `Closed | `Piped ] Fiber.Ivar.t = Fiber.Ivar.create () in
        Fiber.async (fun () ->
            Fiber.npick
              [
                (fun () ->
                  (Fiber.wait state :> [ `Piped | `Closed | `Timeout ] Fiber.t));
                (fun () ->
                  Fiber.sleep timeout >>= fun () -> Fiber.return `Timeout);
              ]
            >>= function
            | `Piped | `Closed -> Fiber.return ()
            | `Timeout ->
                Log.warn (fun m -> m "%s timeout" peer0);
                queue := Psq.remove peer0 !queue;
                queue := Psq.remove peer1 !queue;
                Hashtbl.remove ready peer0;
                Hashtbl.remove ready peer1;
                Bob.Secured.rem_peers t peer0 peer1;
                if Fiber.Ivar.is_empty state then Fiber.Ivar.fill state `Closed;
                Fiber.close fd);
        Hashtbl.add ready peer0 (fd, state);
        Fiber.return ())
      else (* Fiber.Ivar.get state = Some `Closed *) Fiber.return ()

let create_secure_room () = Bob.Secured.make ()

type income =
  [ `Closed
  | `Error of [ `Rd of Unix.error ]
  | `Read of [ `End | `Data of string * int * int ] ]

let pipe fd0 fd1 =
  let closed : [ `Closed ] Fiber.Ivar.t = Fiber.Ivar.create () in
  let rec transmit fd0 fd1 () =
    let peer0 = Unix.getpeername fd0 in
    let peer1 = Unix.getpeername fd1 in

    Fiber.npick
      [
        (fun () -> (Fiber.wait closed :> income Fiber.t));
        (fun () -> (Fiber.read fd0 >>| map_read :> income Fiber.t));
      ]
    >>= function
    | `Closed | `Read `End ->
        if Fiber.Ivar.is_empty closed then Fiber.Ivar.fill closed `Closed;
        Fiber.return ()
    | `Error _ ->
        Log.err (fun m ->
            m "Got an error while reading into %a." pp_sockaddr peer0);
        if Fiber.Ivar.is_empty closed then Fiber.Ivar.fill closed `Closed;
        Fiber.return ()
    | `Read (`Data (str, off, len)) -> (
        Log.debug (fun m ->
            m "[%10s -> %10s]: @[<hov>%a@]"
              (Fmt.str "%a" pp_sockaddr peer0)
              (Fmt.str "%a" pp_sockaddr peer1)
              (Hxd_string.pp Hxd.default)
              (String.sub str off len));
        full_write fd1 str ~off ~len >>= function
        | Ok () -> transmit fd0 fd1 ()
        | Error _ ->
            Log.err (fun m ->
                m "Got an error while writing into %a." pp_sockaddr peer1);
            if Fiber.Ivar.is_empty closed then Fiber.Ivar.fill closed `Closed;
            Fiber.return ())
  in
  let close () =
    Fiber.wait closed >>= fun `Closed ->
    Fiber.close fd0 >>= fun () ->
    Fiber.close fd1 >>= fun () -> Fiber.return ()
  in
  fork_and_join
    (fun () -> fork_and_join (transmit fd0 fd1) (transmit fd1 fd0))
    close
  >>= fun (((), ()), ()) -> Fiber.return ()

let only_if_not_closed ivar fn =
  match Fiber.Ivar.get ivar with
  | Some `Closed -> Fiber.return ()
  | Some `Piped | None -> fn ()

let secure_room ?(timeout = 3600.) socket t ~stop =
  let queue = ref Psq.empty in
  let ready = Hashtbl.create 0x100 in
  let rec create_room () =
    Fiber.npick
      [
        (fun () -> Fiber.wait stop >>= fun () -> Fiber.return `Stop);
        (fun () -> Fiber.return (`Pop (Psq.pop !queue)));
      ]
    >>= function
    | `Stop -> Fiber.return ()
    | `Pop None -> Fiber.pause () >>= create_room
    | `Pop (Some ((_, `Ready), _)) -> Fiber.pause () >>= create_room
    | `Pop (Some ((peer0, `Bound peer1), queue')) -> (
        queue := Psq.remove peer1 queue';
        Log.debug (fun m ->
            m "Start to allocate a secure room for %s and %s." peer0 peer1);
        let fd0, state0 = Hashtbl.find ready peer0 in
        let fd1, state1 = Hashtbl.find ready peer1 in
        Hashtbl.remove ready peer0;
        Hashtbl.remove ready peer1;
        Bob.Secured.rem_peers t peer0 peer1;
        full_write fd0 "00" ~off:0 ~len:2 >>= fun res0 ->
        full_write fd1 "00" ~off:0 ~len:2 >>= fun res1 ->
        match (res0, res1, Fiber.Ivar.get state0, Fiber.Ivar.get state1) with
        | Ok (), Ok (), None, None ->
            Fiber.Ivar.fill state0 `Piped;
            Fiber.Ivar.fill state1 `Piped;
            Fiber.async (fun () -> pipe fd0 fd1);
            create_room ()
        | _ ->
            only_if_not_closed state0 (fun () -> Fiber.close fd0) >>= fun () ->
            only_if_not_closed state1 (fun () -> Fiber.close fd1) >>= fun () ->
            create_room ())
  in

  let handler fd sockaddr =
    Log.debug (fun m -> m "Got a new connection into the secure room.");
    let state : [ `Closed | `Bounded of string * string ] Fiber.Ivar.t =
      Fiber.Ivar.create ()
    in

    Fiber.async (fun () ->
        loop ~timeout t ready queue state fd (Bob.Secured.reader t));
    Fiber.async (fun () ->
        Fiber.npick
          [
            (fun () ->
              (Fiber.wait state
                :> [ `Closed | `Bounded of string * string | `Timeout ] Fiber.t));
            (fun () -> Fiber.sleep timeout >>= fun () -> Fiber.return `Timeout);
          ]
        >>= function
        | `Timeout ->
            if Fiber.Ivar.is_empty state then (
              Log.warn (fun m -> m "Timeout for %a" pp_sockaddr sockaddr);
              Fiber.Ivar.fill state `Closed;
              full_write fd "01" ~off:0 ~len:2 >>= fun _ -> Fiber.close fd)
            else Fiber.return ()
        | `Bounded _ -> Fiber.return ()
        | `Closed ->
            Log.debug (fun m -> m "%a was closed." pp_sockaddr sockaddr);
            Fiber.return ());

    Fiber.return ()
  in
  fork_and_join (fun () -> serve_when_ready ~stop ~handler socket) create_room
  >>= fun ((), ()) -> Fiber.return ()

type error =
  [ `Closed
  | `End
  | `Timeout
  | `Invalid_peer
  | `Unix of Unix.error
  | `Invalid_response ]

let pp_error ppf = function
  | `Closed | `End -> Fmt.string ppf "Connection closed by peer"
  | `Timeout -> Fmt.string ppf "Timeout"
  | `Invalid_peer -> Fmt.string ppf "Invalid peer"
  | `Unix errno -> Fmt.pf ppf "%s" (Unix.error_message errno)
  | `Invalid_response -> Fmt.string ppf "Invalid response from relay"

let init_peer socket ~identity : (unit, [> error ]) result Fiber.t =
  let packet = Fmt.str "%s\n" identity in
  full_write socket packet ~off:0 ~len:(String.length packet) >>= function
  | Error err ->
      Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
  | Ok () -> (
      Fiber.really_read socket 2 >>= function
      | Error err ->
          Fiber.close socket >>= fun () -> Fiber.return (Error (err :> error))
      | Ok "01" ->
          Fiber.close socket >>= fun () -> Fiber.return (Error `Timeout)
      | Ok "02" ->
          Fiber.close socket >>= fun () -> Fiber.return (Error `Invalid_peer)
      | Ok "00" -> Fiber.return (Ok ())
      | Ok _ ->
          Fiber.close socket >>= fun () ->
          Fiber.return (Error `Invalid_response))
