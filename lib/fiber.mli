(* {1:Fiber implementation.}

   [Fiber] is a little module to be able to do some asynchronous computation
   with one core. The interface is pretty close to [lwt] and/or [async] but the
   semantic differs a bit. *)

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

val read   : Unix.file_descr -> [ `Data of string | `End ] t
val write  : Unix.file_descr -> off:int -> len:int -> string -> int t
val close  : Unix.file_descr -> unit t
val accept : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr) t
val sleep  : float -> unit t

(* {2: The entry-point to execute a {!t}. *)

val run : 'a t -> 'a
