open Stdbob

let src = Logs.Src.create "bob.fiber"

module Log = (val Logs.src_log src : Logs.LOG)

type +'a t = ('a -> unit) -> unit

let return x k = k x
let always x _ = return x
let bind t f k = t (fun x -> f x k)
let both a b = bind a (fun a -> bind b (fun b -> return (a, b)))
let ( >>= ) = bind
let ( >>| ) t f k = t (fun x -> k (f x))
let ( >>? ) x f = x >>= function Ok x -> f x | Error err -> return (Error err)
let catch f handler k = try f () k with exn -> handler exn k

module Option = struct
  let iter f = function Some x -> f x | None -> return ()
end

(* Fibers *)

type fiber = Fiber : ('a -> unit t) * 'a -> fiber

let root = Stdbob.LList.make ()

(* Ivar *)

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

module Mutex = struct
  type +'a fiber = 'a t
  type state = Locked of (unit -> unit) Queue.t | Free
  type t = state ref

  let create () = ref Free

  let lock t k =
    match !t with
    | Locked q -> Queue.push k q
    | Free ->
        t := Locked (Queue.create ());
        k ()

  let unlock t =
    match !t with
    | Free -> ()
    | Locked q ->
        Queue.iter (fun f -> f ()) q;
        t := Free
end

module Condition = struct
  type +'a fiber = 'a t
  type t = (unit -> unit) Queue.t
  type mutex = Mutex.t

  let create () = Queue.create ()
  let signal q = match Queue.take_opt q with Some f -> f () | None -> ()
  let broadcast q = Queue.iter (fun f -> f ()) q
  let wait q _mutex k = Queue.push k q
end

let never _k = ()
let wait = Ivar.read

let detach k =
  let ivar = Ivar.create () in
  let fiber =
    Fiber
      ( (fun ivar ->
          let v = k () in
          Ivar.fill ivar v;
          return ()),
        ivar )
  in
  Stdbob.LList.add root fiber;
  wait ivar

let pause () = detach ignore
let async k = (k ()) ignore
let ignore _ = return ()

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

let rec parallel_iter ~f = function
  | [] -> return ()
  | x :: r ->
      fork (fun () -> f x) >>= fun ivar ->
      parallel_iter ~f r >>= fun () -> wait ivar

let rec parallel_map ~f = function
  | [] -> return []
  | x :: r ->
      fork (fun () -> f x) >>= fun ivar ->
      parallel_map ~f r >>= fun r ->
      wait ivar >>= fun x -> return (x :: r)

(* Unix syscalls *)

let openfile fpath flags mode =
  fork (fun () ->
      try return (Ok (Unix.openfile (Bob_fpath.to_string fpath) flags mode))
      with Unix.Unix_error (errno, _, _) -> return (Error errno))
  >>= wait

let prd = Hashtbl.create 0x100

let read ?(len = Stdbob.io_buffer_size) fd :
    ([ `Data of Stdbob.bigstring | `End ], Unix.error) result t =
  match Hashtbl.find_opt prd fd with
  | Some (`Read (ivar, _)) -> Ivar.read ivar
  | _ ->
      let ivar :
          ([ `Data of Stdbob.bigstring | `End ], Unix.error) result Ivar.t =
        Ivar.create ()
      in
      Hashtbl.add prd fd (`Read (ivar, len));
      Ivar.read ivar

let really_read fd len :
    (Stdbob.bigstring, [ `End | `Unix of Unix.error ]) result t =
  if len = 0 then invalid_arg "Impossible to really read 0 byte.";
  match Hashtbl.find_opt prd fd with
  | Some (`Really_read (ivar, _, _, _)) -> Ivar.read ivar
  | _ ->
      Log.debug (fun m ->
          m "Start to really read something into %d." (Obj.magic fd));
      let ivar :
          (Stdbob.bigstring, [ `End | `Unix of Unix.error ]) result Ivar.t =
        Ivar.create ()
      in
      let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
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

let pwr = Hashtbl.create 0x100

let write fd bstr ~off ~len : (int, [ `Closed | `Unix of Unix.error ]) result t
    =
  match Hashtbl.find_opt pwr fd with
  | Some (`Write (_, ivar)) -> Ivar.read ivar
  | _ ->
      let ivar : (int, [ `Closed | `Unix of Unix.error ]) result Ivar.t =
        Ivar.create ()
      in
      Hashtbl.add pwr fd (`Write ((bstr, off, len), ivar));
      Ivar.read ivar

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX name -> Fmt.pf ppf "<%s>" name

external is_windows : unit -> bool = "bob_is_windows" [@@noalloc]
external is_freebsd : unit -> bool = "bob_is_freebsd" [@@noalloc]
external is_linux : unit -> bool = "bob_is_linux" [@@noalloc]
external is_macos : unit -> bool = "bob_is_macos" [@@noalloc]
external set_nonblock : Unix.file_descr -> bool -> unit = "bob_set_nonblock"

let connect fd sockaddr : (unit, Unix.error) result t =
  match Hashtbl.find_opt pwr fd with
  | Some (`Connect (_sockaddr', ivar)) ->
      Ivar.read ivar
      (* TODO(dinosaure): we should check if [sockaddr = _sockaddr']. *)
  | _ ->
      Log.debug (fun m ->
          m "Call to connect() (operating system: %s)" Sys.os_type);
      let ivar : (unit, Unix.error) result Ivar.t = Ivar.create () in
      if is_linux () then (
        Hashtbl.add pwr fd (`Connect (sockaddr, ivar));
        Ivar.read ivar)
      else (
        Log.debug (fun m -> m "Set file-descriptor into a non-blocking mode.");
        set_nonblock fd true;
        try
          Log.debug (fun m -> m "Try to connect to %a" pp_sockaddr sockaddr);
          Unix.connect fd sockaddr;
          Hashtbl.add pwr fd (`Connect (sockaddr, ivar));
          Ivar.read ivar
        with
        | Unix.Unix_error (Unix.EINPROGRESS, _, _)
          when is_freebsd () || is_macos () ->
            Log.debug (fun m -> m "Connection is in progress.");
            Hashtbl.add pwr fd (`Connect (sockaddr, ivar));
            Ivar.read ivar
        | Unix.Unix_error (errno, _, _) ->
            Log.err (fun m ->
                m "Got an error for a non-blocking connect(): %s."
                  (Unix.error_message errno));
            return (Error errno))

let close fd =
  Log.debug (fun m -> m "Close the file-description %d" (Obj.magic fd));
  (try Unix.close fd
   with Unix.Unix_error (errno, f, arg) ->
     Log.err (fun m -> m "%s(%s): %s" f arg (Unix.error_message errno)));
  (match Hashtbl.find_opt prd fd with
  | Some (`Read (ivar, _)) ->
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
  | Some (`Write (_, ivar)) ->
      Hashtbl.remove pwr fd;
      Ivar.fill ivar (Error `Closed)
  | Some (`Connect (_, ivar)) ->
      Hashtbl.remove pwr fd;
      Ivar.fill ivar (Error Unix.EBADF)
  | None -> ());
  return ()

(* Unblocked Unix syscalls *)

(* XXX(dinosaure): to let us to execute syscalls as fast as we can:
   1) we use [Stdbob.bigstring] because, on upon layer, they use that...
   2) we re-implement [read] and [write] with [Stdbob.bigstring] and
      without exception leak! (to be able to annote externals with [noalloc])
   3) the bad case is when we got an error (if we returns [-1]). In such
      case, that's fine to allocate and so on. On upon layer, we will
      probably terminates.
*)

external bigstring_read :
  Unix.file_descr -> Stdbob.bigstring -> int -> int -> int
  = "bob_bigstring_read"
  [@@noalloc]

external retrieve_error : unit -> Unix.error = "bob_retrieve_error"

let sigrd fd =
  match Hashtbl.find_opt prd fd with
  | None -> ()
  | Some (`Read (ivar, len)) -> (
      let bstr = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
      Hashtbl.remove prd fd;
      match bigstring_read fd bstr 0 len with
      | 0 -> Ivar.fill ivar (Ok `End)
      | ret when ret < 0 ->
          let errno = retrieve_error () in
          Log.err (fun m ->
              m "Got an error while reading: %s" (Unix.error_message errno));
          Ivar.fill ivar (Error errno)
      | len -> Ivar.fill ivar (Ok (`Data (Bigarray.Array1.sub bstr 0 len))))
  | Some (`Really_read (ivar, buf, off, len)) -> (
      Hashtbl.remove prd fd;
      match bigstring_read fd buf off len with
      | 0 -> Ivar.fill ivar (Error `End)
      | len' when len = len' -> Ivar.fill ivar (Ok buf)
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
        Stdbob.bigstring_blit_from_bytes src ~src_off dst ~dst_off ~len
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

external bigstring_write :
  Unix.file_descr -> Stdbob.bigstring -> int -> int -> int
  = "bob_bigstring_write"
  [@@noalloc]

let sigwr fd =
  match Hashtbl.find_opt pwr fd with
  | None -> ()
  | Some (`Connect (sockaddr, ivar)) when is_windows () -> (
      Hashtbl.remove pwr fd;
      try
        Unix.connect fd sockaddr;
        set_nonblock fd false;
        Ivar.fill ivar (Ok ())
      with
      | Unix.Unix_error (Unix.EISCONN, _, _) -> Ivar.fill ivar (Ok ())
      | Unix.Unix_error (errno, _, _) -> Ivar.fill ivar (Error errno))
  | Some (`Connect (sockaddr, ivar)) when is_linux () -> (
      Hashtbl.remove pwr fd;
      try
        Unix.connect fd sockaddr;
        Ivar.fill ivar (Ok ())
      with Unix.Unix_error (errno, _, _) -> Ivar.fill ivar (Error errno))
  | Some (`Connect (_sockaddr, ivar)) (* when is_freebsd () || is_macos () *) ->
      Log.debug (fun m -> m "Event from connect() (*BSD connect()).");
      (* TODO(dinosaure): we probably should verify the connection with
         [getpeername], see https://cr.yp.to/docs/connect.html for more
         details about how to retrieve possible error from connect()
         with FreeBSD. *)
      Hashtbl.remove pwr fd;
      set_nonblock fd false;
      Ivar.fill ivar (Ok ())
  | Some (`Write ((bstr, off, len), ivar)) -> (
      let ret = bigstring_write fd bstr off len in
      if ret >= 0 then (
        Hashtbl.remove pwr fd;
        Ivar.fill ivar (Ok ret))
      else
        let errno = retrieve_error () in
        match errno with
        | Unix.ECONNRESET ->
            Hashtbl.remove pwr fd;
            Ivar.fill ivar (Error `Closed)
        | errno ->
            Log.err (fun m ->
                m "write(%d): %s" (Obj.magic fd) (Unix.error_message errno));
            Hashtbl.remove pwr fd;
            Ivar.fill ivar (Error (`Unix errno)))

let bstr_empty = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0

let sigexcept fd =
  Log.debug (fun m -> m "Got an exception from %d" (Obj.magic fd));
  match Hashtbl.find_opt prd fd with
  | None -> ()
  | Some (`Read (ivar, _len)) ->
      Hashtbl.remove prd fd;
      Ivar.fill ivar (Ok (`Data bstr_empty))
  | Some (`Really_read (ivar, _, _, _)) ->
      Hashtbl.remove prd fd;
      Ivar.fill ivar (Error `End)
  | Some (`Accept _) -> Hashtbl.remove prd fd
  | Some (`Getline (queue, ivar)) -> (
      match line_of_queue queue with
      | None ->
          Hashtbl.remove prd fd;
          Ivar.fill ivar None
      | Some line ->
          Hashtbl.remove prd fd;
          Ivar.fill ivar (Some line))

(* Sleepers *)

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

(* First entry point of Fiber *)

let run fiber =
  let result = ref None in
  fiber (fun x -> result := Some x);
  let rec loop () =
    Time.register_sleepers ();
    let rds = Hashtbl.fold (fun socket _ivar rds -> socket :: rds) prd [] in
    let wrs = Hashtbl.fold (fun socket _ivar wrs -> socket :: wrs) pwr [] in
    let fbs = Stdbob.LList.to_list root in
    let slp = Time.sleepers [] in

    (* Log.debug (fun m ->
        m "rds:%d, wrs:%d, fibers:%d, sleepers:%d" (List.length rds)
          (List.length wrs) (List.length fbs) (List.length slp)); *)

    (* XXX(dinosaure): it seems that, on Windows, the EOF is signaled
       via the [except] list of file-descriptors - on Linux, only the
       [rds] list is enough. *)
    let ready_rds, ready_wrs, ready_excepts =
      (* TODO(dinosaure): it seems that on MacOS, fds are catched
         with an exception. Many people say that we should just
         ignore exceptions and only Windows has a special case
         for the [connect()] function. However, we really should take
         a look on that. *)
      try Unix.select rds wrs rds 0.1 with
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

    (* Log.debug (fun m ->
        m "ready rds:%d, ready wrs:%d, ready excepts:%d" (List.length ready_rds)
          (List.length ready_wrs)
          (List.length ready_excepts)); *)
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
    List.iter sigexcept ready_excepts;

    if !result = None then loop ()
  in
  loop ();
  match !result with Some x -> x | None -> failwith "Fiber.run"

(* Cleaner *)

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
