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
           Lwt.wakeup_later u (Ok `Stopped) ;
           Lwt.return_unit) ;
       t in
     let rec loop () =
       accept t >>= function
       | Ok socket ->
           Lwt.async (fun () ->
               handshake socket >>= function
               | Ok flow -> handler flow
               | Error `Closed ->
                   Lwt.return_unit
               | Error _err ->
                   Lwt.return_unit) ;
           loop ()
       | Error `Closed -> Lwt.return_error `Closed
       | Error _ -> Lwt.pause () >>= loop in
     let stop_result =
       Lwt.pick [ switched_off; loop () ] >>= function
       | Ok `Stopped -> close t >>= fun () -> Lwt.return_ok ()
       | Error _ as err -> close t >>= fun () -> Lwt.return err in
     stop_result >>= function Ok () | Error `Closed -> Lwt.return_unit)

module Make (Time : Mirage_time.S) (Stack : Tcpip.Stack.V4V6) = struct
  let service ~port tcp =
    let queue     = Queue.create () in
    let condition = Lwt_condition.create () in
    let mutex     = Lwt_mutex.create () in
    let closed    = ref false in

    let listener flow =
      Lwt_mutex.lock mutex >>= fun () ->
      Queue.push flow queue ;
      Lwt_condition.signal condition () ;
      Lwt_mutex.unlock mutex ;
      Lwt.return_unit in
    Stack.TCP.listen ~port tcp listener ;

    let rec accept () =
      Lwt_mutex.lock mutex >>= fun () ->
      let rec await () =
        if Queue.is_empty queue && not !closed
        then Lwt_condition.wait condition ~mutex >>= await
        else Lwt.return_unit in
      await () >>= fun () -> match Queue.pop queue with
      | flow -> Lwt_mutex.unlock mutex ; Lwt.return_ok flow
      | exception Queue.Empty ->
        if !closed
        then ( Lwt_mutex.unlock mutex ; Lwt.return_error `Closed )
        else ( Lwt_mutex.unlock mutex ; accept () ) in
    let close () = closed := true ; Lwt_condition.signal condition () ;
      Lwt.return_unit in
    accept, close

  type income =
    [ `Stop | `Continue | `Close of string
    | `Write of string * string ]

  type outcome =
    [ `Stop | `Continue ]

  module Make (Flow : Mirage_flow.S) = struct
    let relay ?(timeout= 3600_000_000_000L) ~port ~handshake ?stop tcp rooms =
      let t = Bob.Relay.make () in
      let fds = Hashtbl.create 0x100 in
      let (th_stop : [ `Stop ] Lwt.t), wt_stop = Lwt.wait () in
      Lwt_switch.add_hook stop begin fun () ->
      Lwt.wakeup_later wt_stop `Stop ;
      Lwt.return_unit end ;
      let rec write () =
        Lwt.pick [ (th_stop :> income Lwt.t)
                 ; Lwt.return (Bob.Relay.send_to t) ] >>= function
        | `Stop -> Lwt.return_unit
        | `Continue -> Lwt.pause () >>= write
        | `Close identity ->
          ( match Hashtbl.find_opt fds identity with
          | Some (fd, _) -> Hashtbl.remove fds identity ; Flow.close fd
          | None -> Lwt.return_unit ) >>= write
        | `Write (identity, str) ->
          ( match Hashtbl.find_opt fds identity with
          | None ->
            Bob.Relay.rem_peer t ~identity ;
            write ()
          | Some (fd, _) ->
            Flow.write fd (Cstruct.of_string str) >>= function
            | Ok () -> Lwt.return_unit
            | Error _err ->
              Hashtbl.remove fds identity ;
              Flow.close fd ) >>= write in
      let rec read () =
        Lwt.pick [ (th_stop :> outcome Lwt.t)
                 ; Lwt.return `Continue ] >>= function
        | `Stop -> Lwt.return_unit
        | `Continue ->
          Hashtbl.filter_map_inplace begin fun identity (fd, th) ->
          match Lwt.state th with
          | Lwt.Sleep -> Some (fd, th)
          | Lwt.Fail _ | Lwt.Return `Delete -> None
          | Lwt.Return `Continue ->
            let th () = match Hashtbl.mem fds identity with
              | false -> Lwt.return `Delete
              | true -> Flow.read fd >>= function
                | Error _err -> Lwt.return `Delete
                | Ok `Eof -> Lwt.return `Delete
                | Ok (`Data cs) ->
                  let data = `Data (Cstruct.to_string cs, 0, Cstruct.length cs) in
                  match Bob.Relay.receive_from t ~identity data with
                  | `Close -> Lwt.return `Delete
                  | `Agreement (peer0, peer1) ->
                    Bob.Secured.add_peers rooms peer0 peer1 ;
                    Lwt.return `Continue
                  | `Read | `Continue -> Lwt.return `Continue in
            Some (fd, th ()) end fds ;
          Lwt.pause () >>= read in
      let handler (fd, (ipaddr, port)) =
        let identity = Fmt.str "%a:%d" Ipaddr.pp ipaddr port in
        Hashtbl.add fds identity (fd, Lwt.return `Continue) ;
        Bob.Relay.new_peer t ~identity ;
        Lwt.async begin fun () ->
          Time.sleep_ns timeout >>= fun () ->
          if Bob.Relay.exists t ~identity
          then Bob.Relay.rem_peer t ~identity ;
          Lwt.return_unit end ;
        Lwt.return_unit in
      let accept, close = service ~port tcp in
      let `Initialized th = serve_when_ready ~accept ~handshake ~close ?stop ~handler () in
      Lwt.join [ th; read (); write () ]
  end

  module Secured = struct
    let cs_02 = Cstruct.of_string "02"
    let cs_01 = Cstruct.of_string "01"
    let cs_00 = Cstruct.of_string "00"

    type income00 =
      [ `Closed | `Piped | `Invalid_peer
      | `Error of Stack.TCP.error
      | `Continue of Bob.Secured.reader
      | `Peer of string * string ]

    let rec loop ready queue (state : [ `Closed | `Piped ] Lwt_mvar.t) fd k =
      Lwt.pick
        [ (Lwt_mvar.take state :> income00 Lwt.t)
        ; Stack.TCP.read fd >|= function
        | Ok `Eof -> (k `End :> income00)
        | Ok (`Data cs) -> (k (`Data (Cstruct.to_string cs, 0, Cstruct.length cs)) :> income00)
        | Error err -> `Error err ] >>= function
      | `Closed | `Piped -> Lwt.return_unit
      | `Continue k -> loop ready queue state fd k
      | `Error _ | `Invalid_peer ->
        if Lwt_mvar.is_empty state
        then ( Lwt_mvar.put state `Closed >>= fun () ->
               Stack.TCP.write fd cs_02 >>= fun _ ->
               Stack.TCP.close fd )
        else Lwt.return_unit
      | `Peer (peer0, peer1) ->
        if Psq.mem peer1 !queue
        then ( queue := Psq.remove peer1 !queue
             ; queue := Psq.add peer0 (`Bound peer1) !queue |> Psq.add peer1 (`Bound peer0) )
        else queue := Psq.add peer0 `Ready !queue ;
        if Lwt_mvar.is_empty state
        then ( Hashtbl.add ready peer0 (fd, state)
             ; Lwt_mvar.put state `Piped )
        else Lwt.return_unit

    type income01 =
      [ `Closed | `Read of [ `Eof | `Data of Cstruct.t ]
      | `Error of Stack.TCP.error ]

    let pipe fd0 fd1 =
      let closed : [> `Closed ] Lwt_mvar.t = Lwt_mvar.create_empty () in
      let rec transmit fd0 fd1 =
        Lwt.pick [ (Lwt_mvar.take closed :> income01 Lwt.t)
                 ; Stack.TCP.read fd0 >|= function
        | Ok v -> `Read v
        | Error err -> `Error err ] >>= function
        | `Closed | `Read `Eof ->
          if Lwt_mvar.is_empty closed
          then Lwt_mvar.put closed `Closed
          else Lwt.return_unit
        | `Error _ ->
          if Lwt_mvar.is_empty closed
          then Lwt_mvar.put closed `Closed
          else Lwt.return_unit
        | `Read (`Data cs) ->
          Stack.TCP.write fd1 cs >>= function
          | Ok () -> transmit fd0 fd1
          | Error _err ->
            if Lwt_mvar.is_empty closed
            then Lwt_mvar.put closed `Closed
            else Lwt.return_unit in
      let close () = Lwt_mvar.take closed >>= fun _ ->
        Stack.TCP.close fd0 >>= fun () ->
        Stack.TCP.close fd1 >>= fun () ->
        Lwt.return_unit in
      Lwt.join [ transmit fd0 fd1
               ; transmit fd1 fd0
               ; close () ]

    let only_if_not_closed state fn =
      match Lwt.state (Lwt_mvar.take state) with
      | Lwt.Return `Closed -> Lwt.return_unit
      | Lwt.Return `Piped | Lwt.Fail _ | Lwt.Sleep -> fn ()

    type outcome =
      [ `Stop | `Pop of ((string * [ `Ready | `Bound of string ]) * Psq.t) option ]

    let secure_room ?(timeout= 3600_000_000_000L) ~port ?stop tcp rooms =
      let queue = ref Psq.empty in
      let ready = Hashtbl.create 0x100 in
      let (th_stop : [ `Stop ] Lwt.t), wt_stop = Lwt.wait () in
      Lwt_switch.add_hook stop begin fun () ->
      Lwt.wakeup_later wt_stop `Stop ;
      Lwt.return_unit end ;
      let rec create_room () =
        Lwt.pick [ (th_stop :> outcome Lwt.t)
                 ; Lwt.return (`Pop (Psq.pop !queue)) ] >>= function
        | `Stop -> Lwt.return_unit
        | `Pop None -> Lwt.pause () >>= create_room
        | `Pop (Some ((_, `Ready), _)) -> Lwt.pause () >>= create_room
        | `Pop (Some ((peer0, `Bound peer1), queue')) ->
          queue := Psq.remove peer1 queue' ;
          let fd0, state0 = Hashtbl.find ready peer0 in
          let fd1, state1 = Hashtbl.find ready peer1 in
          Bob.Secured.rem_peers rooms peer0 peer1 ;
          Stack.TCP.write fd0 cs_00 >>= fun res0 ->
          Stack.TCP.write fd1 cs_00 >>= fun res1 ->
          match res0, res1 with
          | Ok (), Ok () ->
            Lwt.async (fun () -> pipe fd0 fd1) ;
            Lwt.return_unit
          | _ ->
            only_if_not_closed state0 begin fun () -> Stack.TCP.close fd0 end >>= fun () ->
            only_if_not_closed state1 begin fun () -> Stack.TCP.close fd1 end >>= fun () ->
            Lwt.return_unit in

      let handler fd =
        let state : [ `Closed | `Piped ] Lwt_mvar.t = Lwt_mvar.create_empty () in
        Lwt.async begin fun () -> loop ready queue state fd (Bob.Secured.reader rooms) end ;
        Lwt.async begin fun () ->
        Lwt.pick [ (Lwt_mvar.take state :> [ `Closed | `Piped | `Timeout ] Lwt.t)
                   ; Time.sleep_ns timeout >>= fun () -> Lwt.return `Timeout ]
          >>= function
          | `Timeout ->
            if Lwt_mvar.is_empty state
            then ( Lwt_mvar.put state `Closed >>= fun () ->
                   Stack.TCP.write fd cs_01 >>= fun _ ->
                   Stack.TCP.close fd )
            else Lwt.return_unit
          | `Closed | `Piped -> Lwt.return_unit end ;
        Lwt.return_unit in
      let accept, close = service ~port tcp in
      let handshake fd = Lwt.return_ok fd in
      let `Initialized th = serve_when_ready ~accept ~handshake ~close ?stop ~handler () in
      Lwt.join [ th; create_room () ]
  end

  module Bob_clear = Make (Stack.TCP)

  let start _time stack =
    let rooms = Bob.Secured.make () in
    let handshake socket = Lwt.return (Ok (socket, Stack.TCP.dst socket)) in
    Lwt.join
      [ Bob_clear.relay ~port:(Key_gen.port ()) ~handshake (Stack.tcp stack) rooms
      ; Secured.secure_room ~port:(Key_gen.secure_port ()) (Stack.tcp stack) rooms ]
end
