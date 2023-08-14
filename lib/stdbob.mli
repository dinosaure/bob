val flip : 'a * 'b -> 'b * 'a
val rev : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val identity : 'a -> 'a
val always : 'a -> 'b -> 'a
val ( <.> ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val msgf : ('a, Format.formatter, unit, [> `Msg of string ]) format4 -> 'a
val reword_error : ('e0 -> 'e1) -> ('a, 'e0) result -> ('a, 'e1) result
val never : 'a -> 'b
val io_buffer_size : int
val reraise : exn -> 'a
val line_of_queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t -> string option

module LList : sig
  type 'a seq
  (** Type of a sequence holding values of type ['a]. *)

  type 'a node
  (** Type of node holding one value of type ['a] in a sequence. *)

  val is_empty : 'a seq -> bool
  (** Returns [true] iff the given sequence if empty. *)

  val remove : 'a node -> unit
  (** Removes a node from the sequence it is part of. It does nothing if the
      node has already been removed. *)

  val pop : 'a seq -> 'a option
  (** [pop seq] removes and returns [Some v] where [v] is the rightmost element
      of [seq] or [None] if [seq] is empty. *)

  val to_list : 'a seq -> 'a list

  val add : 'a seq -> 'a -> unit
  (** [add seq v] adds [v] to left of the sequence [seq]. *)

  val make : unit -> _ seq
  (** [make ()] creats a new empty sequence. *)
end

val bytes_to_size : ?decimals:int -> int Fmt.t
val cuts : sep:string -> string -> string list
