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
    ]} *)

type +'a t

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val both : 'a t -> 'b t -> ('a * 'b) t

val fork_and_join :
  (unit -> 'a t) ->
  (unit -> 'b t) ->
  ('a * 'b) t

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

val fork   : (unit -> 'a t) -> 'a Ivar.t t
val pure   : (unit -> 'a) -> 'a t
val wait   : 'a Ivar.t -> 'a t
val pick   : (unit -> 'a t) -> (unit -> 'a t) -> 'a t
val never  : 'a t
val npick  : (unit -> 'a t) list -> 'a t
val pause  : unit -> unit t
val async  : (unit -> 'a t) -> unit
val detach : (unit -> 'a t) -> 'a Ivar.t

(* {2: Unix operations.} *)

val read    : Unix.file_descr -> [ `Data of string | `End ] t
val write   : Unix.file_descr -> off:int -> len:int -> string -> (int, [ `Closed | `Unix of Unix.error ]) result t
val close   : Unix.file_descr -> unit t
val accept  : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr) t
val sleep   : float -> unit t
val getline : Unix.file_descr -> string option t

(* {2: The entry-point to execute a {!t}. *)

val run : 'a t -> 'a
