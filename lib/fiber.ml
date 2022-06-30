let src = Logs.Src.create "bob.fiber"

module Log = (val Logs.src_log src : Logs.LOG)

type +'a t = ('a -> unit) -> unit

let identity x = x
let return x k = k x
let bind t f k = t (fun x -> f x k)
let both a b = bind a (fun a -> bind b (fun b -> return (a, b)))
let ( >>= ) = bind
let ( >>| ) t f k = t (fun x -> k (f x))

type fiber = Fiber : ('a -> unit t) * 'a -> fiber
type seq = { mutable prev : seq; mutable next : seq }

type node = {
  mutable node_prev : seq;
  mutable node_next : seq;
  v : fiber;
  mutable active : bool;
}

external node_of_seq : seq -> node = "%identity"
external seq_of_node : node -> seq = "%identity"

let is_empty seq = seq.next == seq

let remove node =
  if node.active then (
    node.active <- true;
    let seq = seq_of_node node in
    seq.prev.next <- seq.next;
    seq.next.prev <- seq.prev)

let pop seq =
  if is_empty seq then None
  else
    let res = node_of_seq seq.next in
    remove res;
    Some res.v

let to_list seq =
  let rec go acc =
    match pop seq with None -> List.rev acc | Some x -> go (x :: acc)
  in
  go []

let add seq fiber =
  let node =
    { node_prev = seq.prev; node_next = seq; v = fiber; active = true }
  in
  seq.prev.next <- seq_of_node node;
  seq.prev <- seq_of_node node

module Ivar = struct
  type 'a state = Full of 'a | Empty of ('a -> unit) Queue.t
  type 'a t = 'a state ref

  let create () = ref (Empty (Queue.create ()))

  let fill t x =
    match !t with
    | Full _ -> failwith "Ivar.fill"
    | Empty q ->
        t := Full x;
        Queue.iter (fun f -> f x) q

  let read t k = match !t with Full x -> k x | Empty q -> Queue.push k q
  let is_empty t = match !t with Empty _ -> true | _ -> false
  let get t = match !t with Empty _ -> None | Full x -> Some x
  let full v = ref (Full v)
end

let root =
  let rec seq = { prev = seq; next = seq } in
  seq

let never _k = ()
let wait = Ivar.read

let detach k =
  let ivar = Ivar.create () in
  let fiber =
    Fiber
      ( (fun ivar ->
          k () >>= fun v ->
          Ivar.fill ivar v;
          return ()),
        ivar )
  in
  add root fiber;
  ivar

let pause () = detach (fun () -> return ()) |> wait
let async k = (k ()) ignore

let fork f k =
  let ivar = Ivar.create () in
  f () (fun x -> Ivar.fill ivar x);
  k ivar

let pure f = fork (fun () -> return (f ())) >>= wait

let pick f g k =
  let ivar = Ivar.create () in
  f () (fun x -> if Ivar.is_empty ivar then Ivar.fill ivar x);
  g () (fun x -> if Ivar.is_empty ivar then Ivar.fill ivar x);
  Ivar.read ivar k

let npick lst k =
  let ivar = Ivar.create () in
  List.iter
    (fun f -> f () (fun x -> if Ivar.is_empty ivar then Ivar.fill ivar x))
    lst;
  Ivar.read ivar k

let fork_and_join f g =
  fork f >>= fun a ->
  fork g >>= fun b -> both (Ivar.read a) (Ivar.read b)

let prd = Hashtbl.create 0x100
let pwr = Hashtbl.create 0x100

let read fd : ([ `Data of string | `End ], Unix.error) result t =
  match Hashtbl.find_opt prd fd with
  | Some (`Read ivar) -> Ivar.read ivar
  | _ ->
      Log.debug (fun m -> m "Start to read something into %d." (Obj.magic fd));
      let ivar : ([ `Data of string | `End ], Unix.error) result Ivar.t =
        Ivar.create ()
      in
      Hashtbl.add prd fd (`Read ivar);
      Ivar.read ivar

let really_read fd len : (string, [ `End | `Unix of Unix.error ]) result t =
  if len = 0 then invalid_arg "Impossible to really read 0 byte.";
  match Hashtbl.find_opt prd fd with
  | Some (`Really_read (ivar, _, _, _)) -> Ivar.read ivar
  | _ ->
      Log.debug (fun m ->
          m "Start to really read something into %d." (Obj.magic fd));
      let ivar : (string, [ `End | `Unix of Unix.error ]) result Ivar.t =
        Ivar.create ()
      in
      let buf = Bytes.create len in
      Hashtbl.add prd fd (`Really_read (ivar, buf, 0, len));
      Ivar.read ivar

let getline fd : string option t =
  match Hashtbl.find_opt prd fd with
  | Some (`Getline (_, ivar)) -> Ivar.read ivar
  | _ ->
      let ivar : string option Ivar.t = Ivar.create () in
      let queue = Ke.Rke.create ~capacity:0x100 Bigarray.char in
      Hashtbl.add prd fd (`Getline (queue, ivar));
      Ivar.read ivar

let accept fd : (Unix.file_descr * Unix.sockaddr) t =
  match Hashtbl.find_opt prd fd with
  | Some (`Accept ivar) -> Ivar.read ivar
  | _ ->
      let ivar : (Unix.file_descr * Unix.sockaddr) Ivar.t = Ivar.create () in
      Hashtbl.add prd fd (`Accept ivar);
      Ivar.read ivar

let write fd ~off ~len str : (int, [ `Closed | `Unix of Unix.error ]) result t =
  match Hashtbl.find_opt pwr fd with
  | Some (_, ivar) -> Ivar.read ivar
  | None ->
      let ivar : (int, [ `Closed | `Unix of Unix.error ]) result Ivar.t =
        Ivar.create ()
      in
      Hashtbl.add pwr fd ((str, off, len), ivar);
      Ivar.read ivar

let line_of_queue queue =
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len
  in
  let exists ~p queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter
      (fun chr ->
        if p chr && !res = -1 then res := !pos;
        incr pos)
      queue;
    if !res = -1 then None else Some !res
  in
  match exists ~p:(( = ) '\n') queue with
  | None -> None
  | Some 0 ->
      Ke.Rke.N.shift_exn queue 1;
      Some ""
  | Some pos -> (
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp;
      Ke.Rke.N.shift_exn queue (pos + 1);
      match Bytes.get tmp (pos - 1) with
      | '\r' -> Some (Bytes.sub_string tmp 0 (pos - 1))
      | _ -> Some (Bytes.unsafe_to_string tmp))

let close fd =
  Log.debug (fun m -> m "Close the file-description %d" (Obj.magic fd));
  (try Unix.close fd
   with Unix.Unix_error (errno, f, arg) ->
     Log.err (fun m -> m "%s(%s): %s" f arg (Unix.error_message errno)));
  (match Hashtbl.find_opt prd fd with
  | Some (`Read ivar) ->
      Hashtbl.remove prd fd;
      Ivar.fill ivar (Ok `End)
  | Some (`Really_read (ivar, _, _, _)) ->
      Hashtbl.remove prd fd;
      Ivar.fill ivar (Error `End)
  | Some (`Accept _) -> Hashtbl.remove prd fd
  | Some (`Getline (queue, ivar)) ->
      Hashtbl.remove prd fd;
      Ivar.fill ivar (line_of_queue queue)
  | None -> ());
  (match Hashtbl.find_opt pwr fd with
  | Some (_, ivar) ->
      Hashtbl.remove pwr fd;
      Ivar.fill ivar (Error `Closed)
  | None -> ());
  return ()

let sigrd fd =
  match Hashtbl.find_opt prd fd with
  | None -> ()
  | Some (`Read ivar) -> (
      let buf = Bytes.create 0x100 in
      Hashtbl.remove prd fd;
      match Unix.read fd buf 0 0x100 with
      | 0 -> Ivar.fill ivar (Ok `End)
      | len -> Ivar.fill ivar (Ok (`Data (Bytes.sub_string buf 0 len)))
      | exception Unix.Unix_error (errno, _, _) ->
          Log.err (fun m ->
              m "Got an error while reading: %s" (Unix.error_message errno));
          Ivar.fill ivar (Error errno))
  | Some (`Really_read (ivar, buf, off, len)) -> (
      Hashtbl.remove prd fd;
      match Unix.read fd buf off len with
      | 0 -> Ivar.fill ivar (Error `End)
      | len' when len = len' -> Ivar.fill ivar (Ok (Bytes.unsafe_to_string buf))
      | len' ->
          Hashtbl.add prd fd (`Really_read (ivar, buf, off + len', len - len'))
      | exception Unix.Unix_error (errno, _, _) ->
          Ivar.fill ivar (Error (`Unix errno)))
  | Some (`Accept ivar) ->
      let peer, sockaddr = Unix.accept ~cloexec:true fd in
      Hashtbl.remove prd fd;
      Ivar.fill ivar (peer, sockaddr)
  | Some (`Getline (queue, ivar)) -> (
      let buf = Bytes.create 0x100 in
      let len = Unix.read fd buf 0 0x100 in
      let blit src src_off dst dst_off len =
        Bigstringaf.blit_from_bytes src ~src_off dst ~dst_off ~len
      in
      Ke.Rke.N.push queue ~blit ~length:Bytes.length ~off:0 ~len buf;
      match (len, line_of_queue queue) with
      | 0, None ->
          Hashtbl.remove prd fd;
          Ivar.fill ivar None
      | _, None -> Hashtbl.add prd fd (`Getline (queue, ivar))
      | _, Some line ->
          Hashtbl.remove prd fd;
          Ivar.fill ivar (Some line))

let sigwr fd =
  match Hashtbl.find_opt pwr fd with
  | None -> ()
  | Some ((str, off, len), ivar) -> (
      try
        let len = Unix.write_substring fd str off len in
        Hashtbl.remove pwr fd;
        Ivar.fill ivar (Ok len)
      with
      | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
          Hashtbl.remove pwr fd;
          Ivar.fill ivar (Error `Closed)
      | Unix.Unix_error (errno, f, arg) ->
          Log.err (fun m -> m "%s(%s): %s" f arg (Unix.error_message errno));
          Hashtbl.remove pwr fd;
          Ivar.fill ivar (Error (`Unix errno)))

module Time = struct
  type t = float
  type sleep = { time : t; ivar : unit Ivar.t }

  module Queue = Binary_heap.Make (struct
    type t = sleep

    let compare { time = t1; _ } { time = t2; _ } = Float.compare t1 t2
  end)

  let sleep_queue =
    let dummy = { time = Unix.gettimeofday (); ivar = Ivar.create () } in
    Queue.create ~dummy 0

  let new_sleeps = ref []

  let sleep v =
    let ivar = Ivar.create () in
    let time = Float.add (Unix.gettimeofday ()) v in
    let sleeper = { time; ivar } in
    new_sleeps := sleeper :: !new_sleeps;
    wait ivar

  let rec sleepers acc =
    match Queue.minimum sleep_queue with
    | exception Binary_heap.Empty -> List.rev acc
    | { time; ivar } when time = 0. || time < Unix.gettimeofday () ->
        Queue.remove sleep_queue;
        sleepers (ivar :: acc)
    | _ -> List.rev acc

  let register_sleepers () =
    List.iter (Queue.add sleep_queue) !new_sleeps;
    new_sleeps := []
end

let sleep = Time.sleep

module Set = Set.Make (struct
  type t = Unix.file_descr

  let compare a b = Obj.magic a - Obj.magic b
end)

let () =
  at_exit (fun () ->
      let fds = Set.empty in
      let fds =
        Hashtbl.fold
          (fun socket -> function
            | `Accept _ ->
                identity
                (* XXX(dinosaure): [`Accept] are closed by the relay properly at the end of its process. *)
            | _ -> Set.add socket)
          prd fds
      in
      let fds = Hashtbl.fold (fun socket _ fds -> Set.add socket fds) pwr fds in
      Log.debug (fun m ->
          m "Close remaining file-descriptions: %a"
            Fmt.(Dump.list (using Obj.magic int))
            (Set.elements fds));
      Set.iter
        (fun fd ->
          if fd <> Unix.stdin && fd <> Unix.stdout && fd <> Unix.stderr then (
            Log.debug (fun m -> m "Close %d file-descriptor." (Obj.magic fd));
            Unix.close fd))
        fds)

let run fiber =
  let result = ref None in
  fiber (fun x -> result := Some x);
  let rec loop () =
    Time.register_sleepers ();
    let rds = Hashtbl.fold (fun socket _ivar rds -> socket :: rds) prd [] in
    let wrs = Hashtbl.fold (fun socket _ivar wrs -> socket :: wrs) pwr [] in
    let fbs = to_list root in
    let slp = Time.sleepers [] in

    (* Log.debug (fun m -> m "rds:%d, wrs:%d, fibers:%d, sleepers:%d"
       (List.length rds) (List.length wrs) (List.length fbs) (List.length slp)) ; *)
    let ready_rds, ready_wrs, _others =
      try Unix.select rds wrs [] 0.1 with
      | Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
      | exn ->
          Log.err (fun m ->
              m "Got an exception with rds:@[<hov>%a@] and wrs:@[<hov>%a@]"
                Fmt.(Dump.list (using Obj.magic int))
                rds
                Fmt.(Dump.list (using Obj.magic int))
                wrs);
          raise_notrace exn
    in

    (* Log.debug (fun m -> m "ready rds:%d, ready wrs:%d, ready others:%d"
       (List.length ready_rds) (List.length ready_wrs) (List.length others)) ; *)
    List.iter
      (fun (Fiber (k, ivar)) ->
        try k ivar (fun () -> ())
        with exn ->
          Log.err (fun m ->
              m "Got an unexpected exception: %s" (Printexc.to_string exn)))
      fbs;

    List.iter (fun ivar -> Ivar.fill ivar ()) slp;
    List.iter sigrd ready_rds;
    List.iter sigwr ready_wrs;

    if !result = None then loop ()
  in
  loop ();
  match !result with Some x -> x | None -> failwith "Fiber.run"
