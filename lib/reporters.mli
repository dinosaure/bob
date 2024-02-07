val compression_progress : total:int -> int Progress.Line.t
val progress_bar_for_objects : total:int -> int Progress.Line.t
val progress_bar_for_file : total:int -> int Progress.Line.t
val verify_bar : total:int -> int Progress.Line.t
val extract_bar : total:int -> int Progress.Line.t
val transfer_bar : total:int -> int Progress.Line.t
val incoming_data : int Progress.Line.t
val counter : int Progress.Line.t

val with_reporter :
  config:Progress.Config.t ->
  bool ->
  'a Progress.Line.t ->
  (('a -> unit) * (unit -> unit) -> 'b) ->
  'b
