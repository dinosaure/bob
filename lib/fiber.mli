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

    The best advise is to limit as possible as we can leak of exception. You
    can use {!val:catch} for that when we try to catch an exception from
    an {b effect-full} fiber.

    {2: Introduction.}

    A {b fiber} is a placeholder for a single value which might take a long
    time to compute. Speaking roughly, a fiber is a [ref] that can be filled
    in later. To make that precise, here is how fibers differ from [ref]s:
    - A fiber might not have a value yet. A fiber in this state is called a
      {e pending} fiber.
    - Writing a value into a fiber is called {e resolving} it. A fiber with a
      value is called a {e resolved} fiber.
    - Each fiber can be resolved only once. After a fiber has a value, the
      fiber is immutable.
    - It's possible to attach a {b callback} to a fiber. They will run when the
      fiber has a value, i.e. is resolved. If the fiber is already resolved
      when a callback is attached, the callback is run (almost) right away. If
      the promise is pending, the callback is put into a list and waits.

    So, fibers are optional, write-once references, and when they don't yet
    have a value, they store a list of callbacks that are waiting for the
    value.

    The waiting callbacks make fibers a natural data type for asynchronous
    programming. For example, you can ask [Fiber] to {!val:read} a file.
    [Fiber] immediately returns you a {e fiber} for the data.

    You can neglect this fiber for a while. You can do some other computation,
    request more I/O, etc. At some point, you might decide to attach a callback
    to the {!val:read} fiber, maybe several callbacks.

    In the meantime, the {!val:read} operation is running in the background.
    Once it finishes, [Fiber] {e resolves} the {!val:read} fiber by putting the
    data into it. [Fiber] then runs the callbacks you attached.

    One of those might take the data, and ask [Fiber] to {!val:write} it to
    [<stdout>]. [Fiber] gives you a fiber for that, too, and the process
    repeats.

    {2: Difference with [Lwt]/[Async].}

    [Fiber] wants to be portable for Unix and Windows. Thanks to the
    [Cosmopolitan] project which provides {i syscalls} for any targets. [Lwt]
    and [Async] are more complete than [Fiber] but several works are needed
    to be compatible with [Cosmopolitan].

    For instance, the cancellation does not exist in [Fiber], you can not
    {i cancel} a fiber which is not yet resolved.

    {2: Internal engine.}

    [Fiber] directly provides {!val:run} unlike [Lwt] which proposes multiple
    engines ([libev], [pthread] or [Unix.select]). We only use
    {!val:Unix.select} which is the most portable {i syscalls} for any targets.
*)

type +'a t
(** Fibers for values of type ['a].

    A {b fiber} is a memory cell that is always in one of 2 {b states}:
    - {e fulfilled}, and containing one value of type ['a],
    - {e pending}, in which case it may become fulfilled later.

    Fibers are typically "read" by attaching {b callbacks} to them. The most
    basic functions for that are {!val:bind}, which attaches a callback that is
    called when a promise becomes fulfilled. *)

val return : 'a -> 'a t
(** [return v] creates a new {{!type:t} fiber} that is {e already fulfilled}
    with value [v].

    This is needed to satisfy the type system in some cases. For example, in a
    [match] expression where one case evaluates to a fiber, the other cases
    have to evaluate to fibers as well:

    {[
      match need_input with
      | true -> Fiber.read stdin (* Has type string Fiber.t *)
      | false -> Fiber.return "" (* ...so wrap empty string in a fiber. *)
    ]} *)

val ignore : _ -> unit t
val always : 'a -> _ -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind t0 f] makes it so that [f] will run when [t0] is
    {{!t} {e fulfilled}}.

    When [t0] is fulfilled with value [v], the callback [f] is called with that
    same value [v]. Eventually, after perhaps starting some I/O or other
    computation, [f] returns fiber [t1].

    {!val:bind} itself returns immediatly. It only attaches the callback [f] to
    [t0] - it does not wait for [t1]. {e What} {!val:bind} returns is yet a
    third fiber, [t2]. Rhougly speaking, fulfillment of [t2] represents both
    [t0] and [t1] becoming fulfilled, one after the other. *)

val both : 'a t -> 'b t -> ('a * 'b) t
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t
val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

val ( >>? ) :
  ('a, 'e) result t -> ('a -> ('b, 'e) result t) -> ('b, 'e) result t

module Option : sig
  val iter : ('a -> unit t) -> 'a option -> unit t
end

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
  Bob_fpath.t ->
  Unix.open_flag list ->
  int ->
  (Unix.file_descr, Unix.error) result t

val read :
  ?len:int ->
  Unix.file_descr ->
  ([ `Data of Stdbob.bigstring | `End ], Unix.error) result t

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

val connect : Unix.file_descr -> Unix.sockaddr -> (unit, Unix.error) result t
val close : Unix.file_descr -> unit t
val accept : Unix.file_descr -> (Unix.file_descr * Unix.sockaddr) t
val sleep : float -> unit t
val getline : Unix.file_descr -> string option t

(* {2: The entry-point to execute a {!type:t}. *)

val run : 'a t -> 'a
