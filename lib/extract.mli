open Stdbob
open Bob_protocol

type error =
  [ `Empty_pack_file
  | `Invalid_first_entry
  | `Corrupted_pack_file
  | `Invalid_pack_file
  | `Unexpected_kind_of_document ]

val pp_error : error Fmt.t

val extract :
  quiet:bool ->
  ?g:Random.State.t ->
  ?metadata:Metadata.t ->
  config:Progress.Config.t ->
  bigstring Stream.source ->
  Bob_fpath.t option ->
  (Bob_fpath.t, [> error ]) result Fiber.t
(** [extract ~quiet ?g ?metadata ~config src dst] extracts and return the
    destination where the document was extracted (the file or the directory). *)
