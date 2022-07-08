(** {1:Fiber implementation.}

    [Fiber] is a little module to be able to do some asynchronous computation
    with one core. The interface is pretty close to [lwt] and/or [async] but
    the semantic differs a bit.

    {b NOTE}: the usage of exception with this module is {b broken}. Indeed,
    [Fiber] never tries to catch any exception. So if a fiber raise one, it
    can be lost and catched by the next {i exception catcher}. That mostly
    means that for a fiber 0 which raises an exception and a fiber 1 which
    catch an exception, fiber 1 will retrieve the exception of fiber 0 (even
    if the exception is completely unrelated to what fiber 1 does).

    {[
      [fiber 0] [fiber 1]
           |       |
        raise A  try do something (* exception A appears! *)
           |     with B -> ...
           '       |
                 Exception A appear at line: try do something
    ]}

    The best advise is to limit as possible as we can leak of exception.
*)

type +'a t

val return : 'a -> 'a t
val ignore : _ -> unit t
val always : 'a -> _ -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t
val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

module Ivar : sig
  type 'a t

  val create : unit -> 'a t
  val fill : 'a t -> 'a -> unit
  val is_empty : 'a t -> bool
  val get : 'a t -> 'a option
  val full : 'a -> 'a t
end

module Mutex : sig
  type +'a fiber = 'a t
  type t

  val create : unit -> t
  val lock : t -> unit fiber
  val unlock : t -> unit
end

module Condition : sig
  type +'a fiber = 'a t
  type t
  type mutex = Mutex.t

  val create : unit -> t
  val signal : t -> unit
  val broadcast : t -> unit
  val wait : t -> mutex -> unit fiber
end

val fork : (unit -> 'a t) -> 'a Ivar.t t
val pure : (unit -> 'a) -> 'a t
val wait : 'a Ivar.t -> 'a t
val pick : (unit -> 'a t) -> (unit -> 'a t) -> 'a t
val never : 'a t
val npick : (unit -> 'a t) list -> 'a t
val pause : unit -> unit t
val async : (unit -> 'a t) -> unit
val detach : (unit -> 'a) -> 'a t
val parallel_map : f:('a -> 'b t) -> 'a list -> 'b list t
val parallel_iter : f:('a -> unit t) -> 'a list -> unit t

(* {2: Unix operations.} *)

val openfile :
  Fpath.t ->
  Unix.open_flag list ->
  int ->
  (Unix.file_descr, Unix.error) result t

val read :
  Unix.file_descr -> ([ `Data of Stdbob.bigstring | `End ], Unix.error) result t

val write :
  Unix.file_descr ->
  Stdbob.bigstring ->
  off:int ->
  len:int ->
  (int, [ `Closed | `Unix of Unix.error ]) result t

val really_read :
  Unix.file_descr ->
  int ->
  (Stdbob.bigstring, [ `End | `Unix of Unix.error ]) result t

val close : Unix.file_descr -> unit t
val accept : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr) t
val sleep : float -> unit t
val getline : Unix.file_descr -> string option t

(* {2: The entry-point to execute a {!t}. *)

val run : 'a t -> 'a
