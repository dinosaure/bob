open Lwt.Infix

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

let serve_when_ready :
    type t socket flow.
    accept:(t -> (socket, 'error) result Lwt.t) ->
    handshake:(socket -> (flow, 'error) result Lwt.t) ->
    close:(t -> unit Lwt.t) ->
    ?stop:Lwt_switch.t ->
    handler:(flow -> unit Lwt.t) ->
    t ->
    [ `Initialized of unit Lwt.t ] =
 fun ~accept ~handshake ~close ?stop ~handler t ->
  `Initialized
    (let switched_off =
       let t, u = Lwt.wait () in
       Lwt_switch.add_hook stop (fun () ->
           Lwt.wakeup_later u (Ok `Stopped);
           Lwt.return_unit);
       t
     in
     let rec loop () =
       accept t >>= function
       | Ok socket ->
           Lwt.async (fun () ->
               handshake socket >>= function
               | Ok flow -> handler flow
               | Error `Closed -> Lwt.return_unit
               | Error _err -> Lwt.return_unit);
           loop ()
       | Error `Closed -> Lwt.return_error `Closed
       | Error _ -> Lwt.pause () >>= loop
     in
     let stop_result =
       Lwt.pick [ switched_off; loop () ] >>= function
       | Ok `Stopped -> close t >>= fun () -> Lwt.return_ok ()
       | Error _ as err -> close t >>= fun () -> Lwt.return err
     in
     stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)

let always v _ = v

module Make (Time : Mirage_time.S) (Stack : Tcpip.Stack.V4V6) = struct
  let service ~port tcp =
    let queue = Queue.create () in
    let condition = Lwt_condition.create () in
    let mutex = Lwt_mutex.create () in
    let closed = ref false in

    let listener flow =
      Lwt_mutex.lock mutex >>= fun () ->
      Queue.push flow queue;
      Lwt_condition.signal condition ();
      Lwt_mutex.unlock mutex;
      Lwt.return_unit
    in
    Stack.TCP.listen ~port tcp listener;

    let rec accept () =
      Lwt_mutex.lock mutex >>= fun () ->
      let rec await () =
        if Queue.is_empty queue && not !closed then
          Lwt_condition.wait condition ~mutex >>= await
        else Lwt.return_unit
      in
      Logs.debug (fun m -> m "Waiting for a new connection.");
      await () >>= fun () ->
      match Queue.pop queue with
      | flow ->
          Lwt_mutex.unlock mutex;
          Lwt.return_ok flow
      | exception Queue.Empty ->
          if !closed then (
            Lwt_mutex.unlock mutex;
            Lwt.return_error `Closed)
          else (
            Lwt_mutex.unlock mutex;
            accept ())
    in
    let close () =
      closed := true;
      Lwt_condition.signal condition ();
      Lwt.return_unit
    in
    (accept, close)

  let until_timeout ~timeout =
    Time.sleep_ns timeout >>= fun () -> Lwt.return `Timeout

  type income =
    [ `Stop | `Continue | `Close of string | `Write of string * string ]

  type outcome = [ `Stop | `Continue ]

  let wait ~until t condition =
    Lwt_condition.wait condition >|= fun () -> until t

  type writing_event =
    [ `Write of string * string | `Close of string | `Continue ]

  module Make (Flow : Mirage_flow.S) = struct
    let relay ?(timeout = 3600_000_000_000L) ~port ~handshake ?stop tcp rooms =
      let wr_condition = Lwt_condition.create () in
      let rd_condition = Lwt_condition.create () in
      let t = Bob.Relay.make () in
      let fds = Hashtbl.create 0x100 in
      let (th_stop : [ `Stop ] Lwt.t), wt_stop = Lwt.wait () in
      Lwt_switch.add_hook stop (fun () ->
          Lwt.wakeup_later wt_stop `Stop;
          Lwt.return_unit);
      let rec write () =
        Logs.debug (fun m ->
            m "Waiting to write something into the Bob's handshake.");
        Lwt.pick
          [
            (th_stop :> income Lwt.t);
            wait ~until:Bob.Relay.send_to t wr_condition;
          ]
        >>= function
        | `Stop -> Lwt.return_unit
        | `Continue -> write ()
        | #writing_event as first ->
            Logs.debug (fun m -> m "Handle writing operations.");
            let rec go = function
              | `Continue -> Lwt.return_unit
              | `Close identity ->
                  (match Hashtbl.find_opt fds identity with
                  | Some (fd, _) ->
                      Hashtbl.remove fds identity;
                      Logs.debug (fun m -> m "Close the connection %S" identity);
                      Flow.close fd
                  | None -> Lwt.return_unit)
                  >>= fun () -> go (Bob.Relay.send_to t)
              | `Write (identity, str) ->
                  (match Hashtbl.find_opt fds identity with
                  | None ->
                      Bob.Relay.rem_peer t ~identity;
                      Lwt.return_unit
                  | Some (fd, _) -> (
                      Flow.write fd (Cstruct.of_string str) >>= function
                      | Ok () -> Lwt.return_unit
                      | Error _err ->
                          Hashtbl.remove fds identity;
                          Logs.debug (fun m ->
                              m "Close the connection %S" identity);
                          Flow.close fd))
                  >>= fun () -> go (Bob.Relay.send_to t)
            in
            go first >>= fun () ->
            Logs.debug (fun m -> m "Signal to handle next event.");
            Lwt_condition.signal rd_condition ();
            write ()
      in
      let rec read () =
        Logs.debug (fun m ->
            m "Waiting to read something from the Bob's handshake.");
        Lwt.pick
          [
            (th_stop :> outcome Lwt.t);
            wait ~until:(always `Continue) () rd_condition;
          ]
        >>= function
        | `Stop -> Lwt.return_unit
        | `Continue ->
            Logs.debug (fun m -> m "Scan readers.");
            Hashtbl.filter_map_inplace
              (fun identity (fd, th) ->
                match Lwt.state th with
                | Lwt.Sleep -> Some (fd, th)
                | Lwt.Fail _ | Lwt.Return `Delete -> None
                | Lwt.Return `Continue ->
                    let th () =
                      match Hashtbl.mem fds identity with
                      | false -> Lwt.return `Delete
                      | true -> (
                          Logs.debug (fun m ->
                              m "Read something from %S." identity);
                          Flow.read fd >>= function
                          | Error _err -> Lwt.return `Delete
                          | Ok `Eof -> Lwt.return `Delete
                          | Ok (`Data cs) -> (
                              let data =
                                `Data
                                  (Cstruct.to_string cs, 0, Cstruct.length cs)
                              in
                              let result =
                                Bob.Relay.receive_from t ~identity data
                              in
                              Logs.debug (fun m ->
                                  m "Signal to handle next event.");
                              Lwt_condition.signal wr_condition ();
                              match result with
                              | `Close -> Lwt.return `Delete
                              | `Agreement (peer0, peer1) ->
                                  Bob.Secured.add_peers rooms peer0 peer1;
                                  Lwt.return `Continue
                              | `Read | `Continue -> Lwt.return `Continue))
                    in
                    Some (fd, th ()))
              fds;
            read ()
      in
      let handler (fd, (ipaddr, port)) =
        let identity = Fmt.str "%a:%d" Ipaddr.pp ipaddr port in
        Hashtbl.add fds identity (fd, Lwt.return `Continue);
        Bob.Relay.new_peer t ~identity;
        Logs.debug (fun m -> m "Open a new connection with %S" identity);
        Lwt_condition.signal rd_condition ();
        Lwt_condition.signal wr_condition ();
        Lwt.async (fun () ->
            Time.sleep_ns timeout >>= fun () ->
            if Bob.Relay.exists t ~identity then Bob.Relay.rem_peer t ~identity;
            Lwt.return_unit);
        Lwt.return_unit
      in
      let accept, close = service ~port tcp in
      let (`Initialized th) =
        serve_when_ready ~accept ~handshake ~close ?stop ~handler ()
      in
      Lwt.join [ th; read (); write () ]
  end

  module Secured = struct
    let cs_02 = Cstruct.of_string "02"
    let cs_01 = Cstruct.of_string "01"
    let cs_00 = Cstruct.of_string "00"

    type income00 =
      [ `Closed
      | `Bounded of string * string
      | `Invalid_peer
      | `Error of Stack.TCP.error
      | `Continue of Bob.Secured.reader
      | `Peer of string * string ]

    let rec loop ~timeout t ready condition queue
        (state : [ `Closed | `Bounded of string * string ] Lwt_mvar.t) fd k =
      Logs.debug (fun m -> m "Handle the main secure loop.");
      Lwt.pick
        [
          (Lwt_mvar.take state :> income00 Lwt.t);
          (Stack.TCP.read fd >|= function
           | Ok `Eof -> (k `End :> income00)
           | Ok (`Data cs) ->
               (k (`Data (Cstruct.to_string cs, 0, Cstruct.length cs))
                 :> income00)
           | Error err -> `Error err);
        ]
      >>= function
      | `Closed | `Bounded _ -> Lwt.return_unit
      | `Continue k -> loop ~timeout t ready condition queue state fd k
      | `Error _ | `Invalid_peer ->
          if Lwt_mvar.is_empty state then
            Lwt_mvar.put state `Closed >>= fun () ->
            Stack.TCP.write fd cs_02 >>= fun _ -> Stack.TCP.close fd
          else Lwt.return_unit
      | `Peer (peer0, peer1) ->
          if Psq.mem peer1 !queue then (
            queue := Psq.remove peer1 !queue;
            queue :=
              Psq.add peer0 (`Bound peer1) !queue
              |> Psq.add peer1 (`Bound peer0);
            Lwt_condition.signal condition ())
          else (
            queue := Psq.add peer0 `Ready !queue;
            Lwt_condition.signal condition ());
          if Lwt_mvar.is_empty state then (
            let state' : [ `Closed | `Piped ] Lwt_mvar.t =
              Lwt_mvar.create_empty ()
            in
            Hashtbl.add ready peer0 (fd, state');
            Lwt_mvar.put state (`Bounded (peer0, peer1)) >>= fun () ->
            Lwt.async (fun () ->
                Lwt.pick
                  [
                    until_timeout ~timeout;
                    (Lwt_mvar.take state'
                      :> [ `Timeout | `Closed | `Piped ] Lwt.t);
                  ]
                >>= function
                | `Piped | `Closed -> Lwt.return_unit
                | `Timeout ->
                    queue := Psq.remove peer0 !queue;
                    queue := Psq.remove peer1 !queue;
                    Lwt_condition.signal condition ();
                    Hashtbl.remove ready peer0;
                    Hashtbl.remove ready peer1;
                    Bob.Secured.rem_peers t peer0 peer1;
                    if Lwt_mvar.is_empty state' then
                      Lwt_mvar.put state' `Closed >>= fun () ->
                      Stack.TCP.close fd
                    else Lwt.return_unit);
            Lwt.return_unit)
          else Lwt.return_unit

    type income01 =
      [ `Closed
      | `Read of [ `Eof | `Data of Cstruct.t ]
      | `Error of Stack.TCP.error ]

    let close_packet = Cstruct.of_string "\xff\xff\xff\xff"

    let pipe fd0 fd1 =
      let closed : [> `Closed ] Lwt_mvar.t = Lwt_mvar.create_empty () in
      let rec transmit fd0 fd1 =
        Lwt.pick
          [
            (Lwt_mvar.take closed :> income01 Lwt.t);
            (Stack.TCP.read fd0 >|= function
             | Ok v -> `Read v
             | Error err -> `Error err);
          ]
        >>= function
        | `Closed | `Read `Eof ->
            if Lwt_mvar.is_empty closed then Lwt_mvar.put closed `Closed
            else Lwt.return_unit
        | `Error _ ->
            if Lwt_mvar.is_empty closed then Lwt_mvar.put closed `Closed
            else Lwt.return_unit
        | `Read (`Data cs) -> (
            Stack.TCP.write fd1 cs >>= function
            | Ok () -> transmit fd0 fd1
            | Error _err ->
                if Lwt_mvar.is_empty closed then Lwt_mvar.put closed `Closed
                else Lwt.return_unit)
      in
      let close () =
        Lwt_mvar.take closed >>= fun _ ->
        Logs.debug (fun m -> m "Pipe closed.");
        Stack.TCP.close fd0 >>= fun () ->
        Stack.TCP.close fd1 >>= fun () -> Lwt.return_unit
      in
      Lwt.join [ transmit fd0 fd1; transmit fd1 fd0 ] >>= fun () ->
      Stack.TCP.write fd0 close_packet >>= fun _ ->
      Stack.TCP.write fd1 close_packet >>= fun _ ->
      close () >>= fun () ->
      Logs.debug (fun m -> m "Pipe finished.");
      Lwt.return_unit

    let only_if_not_closed state fn =
      match Lwt_mvar.take_available state with
      | Some `Closed -> Lwt.return_unit
      | Some `Piped | None -> fn ()

    type outcome =
      [ `Stop | `Pop of (string * [ `Ready | `Bound of string ]) * Psq.t ]

    let wait queue mutex condition =
      Lwt_mutex.lock mutex >>= fun () ->
      let rec await () =
        match Psq.pop !queue with
        | None -> Lwt_condition.wait condition ~mutex >>= await
        | Some v -> Lwt.return v
      in
      await () >>= fun v ->
      Lwt_mutex.unlock mutex;
      Lwt.return (`Pop v)

    let secure_room ?(timeout = 3600_000_000_000L) ~port ?stop tcp rooms =
      let queue = ref Psq.empty in
      let ready = Hashtbl.create 0x100 in
      let (th_stop : [ `Stop ] Lwt.t), wt_stop = Lwt.wait () in
      Lwt_switch.add_hook stop (fun () ->
          Lwt.wakeup_later wt_stop `Stop;
          Lwt.return_unit);
      let condition = Lwt_condition.create () in
      let mutex = Lwt_mutex.create () in
      let rec create_room () =
        Logs.debug (fun m -> m "Waiting for a new room.");
        Lwt.pick [ (th_stop :> outcome Lwt.t); wait queue mutex condition ]
        >>= function
        | `Stop -> Lwt.return_unit
        | `Pop ((_, `Ready), _) -> Lwt.pause () >>= create_room
        | `Pop ((peer0, `Bound peer1), queue') -> (
            queue := Psq.remove peer1 queue';
            let fd0, state0 = Hashtbl.find ready peer0 in
            let fd1, state1 = Hashtbl.find ready peer1 in
            Bob.Secured.rem_peers rooms peer0 peer1;
            Stack.TCP.write fd0 cs_00 >>= fun res0 ->
            Stack.TCP.write fd1 cs_00 >>= fun res1 ->
            match
              ( res0,
                res1,
                Lwt_mvar.take_available state0,
                Lwt_mvar.take_available state1 )
            with
            | Ok (), Ok (), None, None ->
                Lwt_mvar.put state0 `Piped >>= fun () ->
                Lwt_mvar.put state1 `Piped >>= fun () ->
                Lwt.async (fun () -> pipe fd0 fd1);
                create_room ()
            | _ ->
                only_if_not_closed state0 (fun () -> Stack.TCP.close fd0)
                >>= fun () ->
                only_if_not_closed state1 (fun () -> Stack.TCP.close fd1)
                >>= fun () -> create_room ())
      in

      let handler fd =
        let state : [ `Closed | `Bounded of string * string ] Lwt_mvar.t =
          Lwt_mvar.create_empty ()
        in
        Lwt.async (fun () ->
            loop ~timeout rooms ready condition queue state fd
              (Bob.Secured.reader rooms));
        Lwt.async (fun () ->
            Lwt.pick
              [
                until_timeout ~timeout;
                (Lwt_mvar.take state
                  :> [ `Closed | `Bounded of string * string | `Timeout ] Lwt.t);
              ]
            >>= function
            | `Timeout ->
                if Lwt_mvar.is_empty state then
                  Lwt_mvar.put state `Closed >>= fun () ->
                  Stack.TCP.write fd cs_01 >>= fun _ -> Stack.TCP.close fd
                else Lwt.return_unit
            | `Bounded (a, b) ->
                Logs.debug (fun m -> m "%s and %s are bounded." a b);
                Lwt.return_unit
            | `Closed -> Lwt.return_unit);
        Lwt.return_unit
      in
      let accept, close = service ~port tcp in
      let handshake fd = Lwt.return_ok fd in
      let (`Initialized th) =
        serve_when_ready ~accept ~handshake ~close ?stop ~handler ()
      in
      Lwt.join [ th; create_room () ]
  end

  module Bob_clear = Make (Stack.TCP)

  let start _time stack =
    let rooms = Bob.Secured.make () in
    let handshake socket = Lwt.return (Ok (socket, Stack.TCP.dst socket)) in
    Lwt.join
      [
        Bob_clear.relay ~port:(Key_gen.port ()) ~handshake (Stack.tcp stack)
          rooms;
        Secured.secure_room ~port:(Key_gen.secure_port ()) (Stack.tcp stack)
          rooms;
      ]
end
