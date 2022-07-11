type 'a source =
  | Source : {
      init : unit -> 's Fiber.t;
      pull : 's -> ('a * 's) option Fiber.t;
      stop : 's -> unit Fiber.t;
    }
      -> 'a source

module Source : sig
  val file : Fpath.t -> Stdbob.bigstring source
  val array : 'a array -> 'a source
  val fold : ('r -> 'a -> 'r Fiber.t) -> 'r -> 'a source -> 'r Fiber.t
  val unfold : 's -> ('s -> ('a * 's) option Fiber.t) -> 'a source
  val iterate : f:('a -> 'a Fiber.t) -> 'a -> 'a source
  val length : 'a source -> int Fiber.t
  val next : 'a source -> ('a * 'a source) option Fiber.t
  val dispose : 'a source -> unit Fiber.t
end

type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's Fiber.t;
      push : 's -> 'a -> 's Fiber.t;
      full : 's -> bool Fiber.t;
      stop : 's -> 'r Fiber.t;
    }
      -> ('a, 'r) sink

module Sink : sig
  val make :
    init:(unit -> 'acc Fiber.t) ->
    push:('acc -> 'a -> 'acc Fiber.t) ->
    ?full:('acc -> bool Fiber.t) ->
    stop:('acc -> 'r Fiber.t) ->
    unit ->
    ('a, 'r) sink

  val is_full : ('a, 'r) sink -> bool Fiber.t
  val zip : ('a, 'r1) sink -> ('a, 'r2) sink -> ('a, 'r1 * 'r2) sink
  val fill : 'a -> (_, 'a) sink
  val push : 'a -> ('a, 'r) sink -> ('a, 'r) sink

  val flat_map :
    ('r1 -> ('a, 'r2) sink Fiber.t) -> ('a, 'r1) sink -> ('a, 'r2) sink

  type ('a, 'b) race = Both of 'a * 'b | Left of 'a | Right of 'b

  val race : ('a, 'r1) sink -> ('a, 'r2) sink -> ('a, ('r1, 'r2) race) sink

  (** {3: Basics.} *)

  val list : ('a, 'a list) sink
  val string : (string, string) sink
  val bigstring : (Stdbob.bigstring, Stdbob.bigstring) sink

  (** {3: Input & Output.} *)

  val stdout : (Stdbob.bigstring, unit) sink
  val file : ?erase:bool -> Fpath.t -> (Stdbob.bigstring, unit) sink
end

type ('a, 'b) flow = { flow : 'r. ('b, 'r) sink -> ('a, 'r) sink } [@@unboxed]

module Flow : sig
  val identity : ('a, 'a) flow
  val map : ('a -> 'b Fiber.t) -> ('a, 'b) flow
  val filter_map : ('a -> 'b option Fiber.t) -> ('a, 'b) flow
  val compose : ('a, 'b) flow -> ('b, 'c) flow -> ('a, 'c) flow
  val ( << ) : ('a, 'b) flow -> ('b, 'c) flow -> ('a, 'c) flow
  val ( >> ) : ('b, 'c) flow -> ('a, 'b) flow -> ('a, 'c) flow

  (** {3: Buffering.} *)

  val bigbuffer : int -> (string, Stdbob.bigstring) flow

  (** {3: Compression.} *)

  val deflate_zlib :
    q:De.Queue.t ->
    w:De.Lz77.window ->
    level:int ->
    (Stdbob.bigstring, Stdbob.bigstring) flow
end

type 'a stream

module Stream : sig
  val empty : 'a stream
  val singleton : 'a -> 'a stream
  val double : 'a -> 'a -> 'a stream
  val concat : 'a stream -> 'a stream -> 'a stream
  val map : ('a -> 'b Fiber.t) -> 'a stream -> 'b stream
  val filter_map : ('a -> 'b option Fiber.t) -> 'a stream -> 'b stream
  val of_iter : (('a -> unit Fiber.t) -> unit Fiber.t) -> 'a stream
  val iterate : f:('a -> 'a Fiber.t) -> 'a -> 'a stream

  (** {3: Basics.} *)

  val of_list : 'a list -> 'a stream
  val to_list : 'a stream -> 'a list Fiber.t
  val of_array : 'a array -> 'a stream
  val to_array : 'a stream -> 'a array Fiber.t
  val to_bigstring : Stdbob.bigstring stream -> Stdbob.bigstring Fiber.t
  val to_string : string stream -> string Fiber.t

  (** {3: Composition.} *)

  val into : ('a, 'b) sink -> 'a stream -> 'b Fiber.t
  val via : ('a, 'b) flow -> 'a stream -> 'b stream
  val from : 'a source -> 'a stream

  (** {3: Input & Output.} *)

  val of_file :
    Fpath.t -> (Stdbob.bigstring stream, [> `Msg of string ]) result Fiber.t

  val stdin : Stdbob.bigstring stream
  val to_file : Fpath.t -> Stdbob.bigstring stream -> unit Fiber.t
  val stdout : Stdbob.bigstring stream -> unit Fiber.t

  module Infix : sig
    val ( >>= ) : 'a stream -> ('a -> 'b stream Fiber.t) -> 'b stream
    val ( ++ ) : 'a stream -> 'a stream -> 'a stream
  end
end
