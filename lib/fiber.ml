let src = Logs.Src.create "bob.fiber"
module Log = (val Logs.src_log src : Logs.LOG)

type +'a t = ('a -> unit) -> unit

let return x k = k x
let bind t f k = t (fun x -> f x k)
let both a b =
  bind a begin fun a ->
  bind b begin fun b -> return (a, b) end end

let ( >>= ) = bind
let ( >>| ) t f k = t (fun x -> k (f x))

type fiber = Fiber : ('a -> unit t) * 'a -> fiber

type seq = { mutable prev : seq; mutable next : seq; }

type node =
  { mutable node_prev : seq
  ; mutable node_next : seq
  ; v : fiber
  ; mutable active : bool }

external node_of_seq : seq -> node = "%identity"
external seq_of_node : node -> seq = "%identity"

let is_empty seq = seq.next == seq

let remove node =
  if node.active then begin
  node.active <- true ;
  let seq = seq_of_node node in
  seq.prev.next <- seq.next ;
  seq.next.prev <- seq.prev end

let pop seq =
  if is_empty seq 
  then None
  else let res = node_of_seq seq.next in
       ( remove res ; Some res.v )

let to_list seq =
  let rec go acc = match pop seq with
    | None -> List.rev acc
    | Some x -> go (x :: acc) in
  go []

let add seq fiber =
  let node = { node_prev= seq.prev
             ; node_next= seq
             ; v= fiber
             ; active= true } in
  seq.prev.next <- seq_of_node node ;
  seq.prev <- seq_of_node node

module Ivar = struct
  type 'a state = Full of 'a | Empty of ('a -> unit) Queue.t
  type 'a t = 'a state ref

  let create () = ref (Empty (Queue.create ()))

  let fill t x = match !t with
    | Full _ -> failwith "Ivar.fill"
    | Empty q ->
      t := Full x ; Queue.iter (fun f -> f x) q

  let read t k = match !t with
    | Full x -> k x
    | Empty q -> Queue.push k q

  let is_empty t = match !t with
    | Empty _ -> true | _ -> false

  let get t = match !t with
    | Empty _ -> None
    | Full x -> Some x

  let full v = ref (Full v)
end

let root =
  let rec seq = { prev= seq; next= seq; } in
  seq

let never _k = ()
let wait = Ivar.read

let detach k =
  let ivar = Ivar.create () in
  let fiber = Fiber ((fun ivar -> k () >>= fun v -> Ivar.fill ivar v ; return ()), ivar) in
  add root fiber ; ivar

let pause () = detach (fun () -> return ()) |> wait
let async k = (k ()) ignore

let fork f k =
  let ivar = Ivar.create () in
  f () (fun x -> Ivar.fill ivar x) ;
  k ivar

let pure f = fork begin fun () -> return (f ()) end >>= wait

let pick f g k =
  let ivar = Ivar.create () in
  f () (fun x -> if Ivar.is_empty ivar then Ivar.fill ivar x) ;
  g () (fun x -> if Ivar.is_empty ivar then Ivar.fill ivar x) ;
  Ivar.read ivar k

let npick lst k =
  let ivar = Ivar.create () in
  List.iter begin fun f ->
  f () (fun x -> if Ivar.is_empty ivar then Ivar.fill ivar x)
  end lst ; Ivar.read ivar k

let fork_and_join f g =
  fork f >>= fun a ->
  fork g >>= fun b -> both (Ivar.read a) (Ivar.read b)

let prd = Hashtbl.create 0x100
let pwr = Hashtbl.create 0x100

let read fd : [ `Data of string | `End ] t =
  match Hashtbl.find_opt prd fd with
  | Some (`Read ivar) -> Ivar.read ivar
  | _ ->
    let ivar : [ `Data of string | `End ] Ivar.t = Ivar.create () in
    Hashtbl.add prd fd (`Read ivar) ;
    Ivar.read ivar

let accept fd : (Unix.file_descr * Unix.sockaddr) t =
  match Hashtbl.find_opt prd fd with
  | Some (`Accept ivar) -> Ivar.read ivar
  | _ ->
    let ivar : (Unix.file_descr * Unix.sockaddr) Ivar.t = Ivar.create () in
    Hashtbl.add prd fd (`Accept ivar) ;
    Ivar.read ivar

let write fd ~off ~len str : int t =
  match Hashtbl.find_opt pwr fd with
  | Some (_, ivar) -> Ivar.read ivar
  | None ->
    let ivar : int Ivar.t = Ivar.create () in
    Hashtbl.add pwr fd ((str, off, len), ivar) ;
    Ivar.read ivar

let close fd = Unix.close fd ; return ()

let sigrd fd =
  match Hashtbl.find_opt prd fd with
  | None -> ()
  | Some (`Read ivar) ->
    let buf = Bytes.create 0x100 in
    Hashtbl.remove prd fd ;
    ( match Unix.read fd buf 0 0x100 with
    | 0 -> Ivar.fill ivar `End
    | len ->
      Ivar.fill ivar (`Data (Bytes.sub_string buf 0 len))
    | exception _ -> Ivar.fill ivar `End )
  | Some (`Accept ivar) ->
    let peer, sockaddr = Unix.accept ~cloexec:true fd in
    Hashtbl.remove prd fd ;
    Ivar.fill ivar (peer, sockaddr)

let sigwr fd =
  match Hashtbl.find_opt pwr fd with
  | None -> ()
  | Some ((str, off, len), ivar) ->
    let len = Unix.write_substring fd str off len in
    Hashtbl.remove pwr fd ;
    Ivar.fill ivar len

module Time = struct
  type t = float
  type sleep = { time : t; ivar : unit Ivar.t; }

  module Queue = Binary_heap.Make (struct
    type t = sleep
    let compare { time= t1; _ } { time= t2; _ } = Float.compare t1 t2
  end)

  let sleep_queue =
    let dummy = { time= Unix.gettimeofday (); ivar= Ivar.create (); } in
    Queue.create ~dummy 0

  let new_sleeps = ref []

  let sleep v =
    let ivar = Ivar.create () in
    let time = Float.add (Unix.gettimeofday ()) v in
    let sleeper = { time; ivar; } in
    new_sleeps := sleeper :: !new_sleeps ; wait ivar

  let rec sleepers acc =
    match Queue.minimum sleep_queue with
    | exception Binary_heap.Empty -> List.rev acc
    | { time; ivar; } when time = 0. || time < Unix.gettimeofday () ->
      Queue.remove sleep_queue ;
      sleepers (ivar :: acc)
    | _ -> List.rev acc

  let register_sleepers () =
    List.iter (Queue.add sleep_queue) !new_sleeps ;
    new_sleeps := []
end

let sleep = Time.sleep

let run fiber =
  let result = ref None in
  fiber (fun x -> result := Some x) ;
  let rec loop () =
    Time.register_sleepers () ;
    let rds = Hashtbl.fold (fun socket _ivar rds -> socket :: rds) prd [] in
    let wrs = Hashtbl.fold (fun socket _ivar wrs -> socket :: wrs) pwr [] in
    let fbs = to_list root in
    let slp = Time.sleepers [] in

    let ready_rds, ready_wrs, _ = Unix.select rds wrs [] 0.1 in

    List.iter (fun (Fiber (k, ivar)) -> k ivar (fun () -> ())) fbs ;
    List.iter (fun ivar -> Ivar.fill ivar ()) slp ;
    List.iter sigrd ready_rds ;
    List.iter sigwr ready_wrs ;

    if !result = None
    then loop () in
  loop () ; match !result with
  | Some x -> x
  | None -> failwith "Fiber.run"
