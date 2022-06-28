let src = Logs.Src.create "bob.crypto"
module Log = (val Logs.src_log src : Logs.LOG)

module type CIPHER_BLOCK = sig
  type key

  val authenticate_encrypt : key:key -> nonce:Cstruct.t ->
    ?adata:Cstruct.t -> Cstruct.t -> Cstruct.t
  val authenticate_decrypt : key:key -> nonce:Cstruct.t ->
    ?adata:Cstruct.t -> Cstruct.t -> Cstruct.t option
  val of_secret : Cstruct.t -> key
  val tag_size : int
end

type 'k cipher_block = (module CIPHER_BLOCK with type key = 'k)

let module_of : type k. k Spoke.aead -> k cipher_block = function
  | Spoke.GCM -> (module Mirage_crypto.Cipher_block.AES.GCM)
  | Spoke.CCM ->
    let module M = struct
      include Mirage_crypto.Cipher_block.AES.CCM
      let of_secret = of_secret ~maclen:16
      let tag_size = 16
    end in (module M)
  | Spoke.ChaCha20_Poly1305 ->
    let module M = struct
      include Mirage_crypto.Chacha20
      let tag_size = Mirage_crypto.Poly1305.mac_size
    end in (module M)

type symmetric = Symmetric
  : { key : 'k; nonce : Cstruct.t
    ; impl : 'k cipher_block } -> symmetric

external xor_into
  : Cstruct.buffer -> src_off:int -> Cstruct.buffer -> dst_off:int -> len:int -> unit
  = "spoke_xor_into_generic_bigarray"

let xor src dst =
  let len = min (Cstruct.length src) (Cstruct.length dst) in
  xor_into (Cstruct.to_bigarray src) ~src_off:0
    (Cstruct.to_bigarray dst) ~dst_off:0 ~len

let xor a b =
  let len = min (Cstruct.length a) (Cstruct.length b) in
  let res = Cstruct.of_string (Cstruct.copy b 0 len) in
  ( xor a res ; res )

let make_nonce nonce seq =
  let seq =
    let len = Cstruct.length nonce in
    let seq =
      let buf = Cstruct.create 8 in
      Cstruct.BE.set_uint64 buf 0 seq ; buf in
    let pad = Cstruct.create (len - 8) in
    Cstruct.append pad seq in
  xor nonce seq

let make_adata len =
  let buf = Cstruct.create 4 in
  Cstruct.BE.set_uint16 buf 0 Spoke.version ;
  Cstruct.BE.set_uint16 buf 2 len ; buf

let encrypt (Symmetric { key; nonce; impl= (module Cipher_block); }) seq buf =
  let nonce = make_nonce nonce seq in
  let adata = make_adata (Cstruct.length buf) in
  Cipher_block.authenticate_encrypt ~key ~adata ~nonce buf

let decrypt (Symmetric { key; nonce; impl= (module Cipher_block); }) seq buf =
  let nonce = make_nonce nonce seq in
  let adata = make_adata (Cstruct.length buf - Cipher_block.tag_size) in
  Cipher_block.authenticate_decrypt ~key ~adata ~nonce buf

open Fiber

type error =
  [ `Rd of Unix.error
  | `Corrupted ]

let pp_error ppf = function
  | `Rd errno -> Fmt.pf ppf "read(): %s" (Unix.error_message errno)
  | `Corrupted -> Fmt.pf ppf "Encrypted data corrupted"

type write_error =
  [ `Closed
  | `Unix of Unix.error ]

let pp_write_error ppf = function
  | `Closed -> Fmt.pf ppf "Connection closed by peer"
  | `Unix errno -> Fmt.pf ppf "write(): %s" (Unix.error_message errno)

type t =
  { fd : Unix.file_descr
  ; recv : symmetric
  ; send : symmetric
  ; recv_record : Cstruct.t
  ; send_record : Cstruct.t
  ; mutable recv_seq : int64
  ; mutable send_seq : int64
  ; recv_queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t
  ; send_queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t }

let symmetric_of_key_nonce_and_cipher key_nonce (Spoke.AEAD aead) =
  let key_len = 32 in
  let nonce_len = 12 in
  let module Cipher_block = (val module_of aead) in
  let key = Cstruct.of_string ~off:0 ~len:key_len key_nonce in
  let key = Cipher_block.of_secret key in
  let nonce = Cstruct.of_string ~off:key_len ~len:nonce_len key_nonce in
  Symmetric { key; nonce; impl= (module Cipher_block) }

let make ~ciphers:(cipher0, cipher1) ~shared_keys:(k0, k1) fd =
  let recv = symmetric_of_key_nonce_and_cipher k0 cipher0 in
  let send = symmetric_of_key_nonce_and_cipher k1 cipher1 in
  let recv_queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let send_queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let recv_record =
    let Symmetric { impl= (module Cipher_block); _ } = recv in
    Cstruct.create (2 + 0xffff + Cipher_block.tag_size) in
  let send_record =
    let Symmetric { impl= (module Cipher_block); _ } = send in
    Cstruct.create (2 + 0xffff + Cipher_block.tag_size) in
  { fd; recv; send
  ; recv_record; send_record
  ; recv_seq= 0L; send_seq= 0L
  ; recv_queue; send_queue }

let get_record record queue symmetric =
  let Symmetric { impl= (module Cipher_block); _ } = symmetric in
  match Ke.Rke.length queue with
  | 0 -> `Await_hdr
  | 1 -> `Await_rec 1
  | 2 | _ ->
    let blit src src_off dst dst_off len =
      let src = Cstruct.of_bigarray src ~off:src_off ~len in
      Cstruct.blit src 0 dst dst_off len in
    Ke.Rke.N.keep_exn queue ~blit ~length:Cstruct.length record ~len:2 ;
    let len = Cstruct.BE.get_uint16 record 0 in
    if Ke.Rke.length queue >= len
    then ( Ke.Rke.N.keep_exn queue ~blit ~length:Cstruct.length record ~len
         ; Ke.Rke.N.shift_exn queue len
         ; `Record (Cstruct.sub record 2 (len - 2)) )
    else `Await_rec (len - Ke.Rke.length queue)

let rec recv flow =
  match get_record flow.recv_record flow.recv_queue flow.recv with
  | `Record buf ->
    ( match decrypt flow.recv flow.recv_seq buf with
    | Some buf ->
      flow.recv_seq <- Int64.succ flow.recv_seq ;
      Fiber.return (Ok (`Data (Cstruct.to_string buf)))
    | None -> Fiber.return (Error `Corrupted) )
  | (`Await_hdr | `Await_rec _) as await ->
    ( Fiber.read flow.fd >>= function
    | Error errno -> Fiber.return (Error (`Rd errno))
    | Ok `End ->
      if await = `Await_hdr
      then Fiber.return (Ok `End)
      else Fiber.return (Error `Corrupted)
    | Ok (`Data str) ->
      let blit src src_off dst dst_off len =
        let dst = Cstruct.of_bigarray dst ~off:dst_off ~len in
        Cstruct.blit_from_string src src_off dst 0 len in
      Ke.Rke.N.push flow.recv_queue ~blit ~length:String.length str ;
      recv flow )

let record ~dst ~seq queue symmetric =
  let len = min 0xffff (Ke.Rke.length queue) in
  let blit src src_off dst dst_off len =
    let src = Cstruct.of_bigarray src ~off:src_off ~len in
    Cstruct.blit src 0 dst dst_off len in
  Ke.Rke.N.keep_exn queue ~length:Cstruct.length ~blit ~off:2 ~len dst ;
  let buf = encrypt symmetric seq (Cstruct.sub dst 2 len) in
  Ke.Rke.N.shift_exn queue len ;
  let len = 2 + Cstruct.length buf in
  Cstruct.BE.set_uint16 dst 0 len ;
  Cstruct.blit buf 0 dst 2 (Cstruct.length buf) ;
  Cstruct.sub dst 0 len

let full_write fd cs =
  let str = Cstruct.to_string cs in
  let rec go str off len =
    Fiber.write fd ~off ~len str >>= function
    | Error _ as err -> Fiber.return err
    | Ok len' ->
      if len - len' > 0
      then go str (off + len') (len - len')
      else Fiber.return (Ok ()) in
  go str 0 (String.length str)

let ( >>? ) x f = x >>= function
  | Ok x -> f x
  | Error _ as err -> Fiber.return err

let rec flush flow =
  if not (Ke.Rke.is_empty flow.send_queue)
  then let record = record
         ~dst:flow.send_record ~seq:flow.send_seq
         flow.send_queue flow.send in
       ( flow.send_seq <- Int64.succ flow.send_seq
       ; full_write flow.fd record >>? fun () ->
         flush flow )
  else Fiber.return (Ok ())

let send flow str ~off ~len =
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len in
  Ke.Rke.N.push flow.send_queue ~blit ~length:String.length ~off ~len str ;
  flush flow >>? fun () -> Fiber.return (Ok len)

let close { fd; _ } = Fiber.close fd

let line_of_queue queue =
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
  let exists ~p queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter begin fun chr ->
    if p chr && !res = -1 then res := !pos ;
    incr pos end queue ;
    if !res = -1 then None else Some !res in
  match exists ~p:((=) '\n') queue with
  | None -> None
  | Some 0 -> Ke.Rke.N.shift_exn queue 1 ; Some ""
  | Some pos ->
    let tmp = Bytes.create pos in
    Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
    Ke.Rke.N.shift_exn queue (pos + 1) ;
    match Bytes.get tmp (pos - 1) with
    | '\r' -> Some (Bytes.sub_string tmp 0 (pos - 1))
    | _ -> Some (Bytes.unsafe_to_string tmp)

let rec getline queue flow = match line_of_queue queue with
  | Some line -> Fiber.return (Some line)
  | None ->
    recv flow >>= function
    | Ok `End -> Fiber.return None
    | Ok (`Data str) ->
      let blit src src_off dst dst_off len =
        Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len in
      Ke.Rke.N.push queue ~blit ~length:String.length str ;
      getline queue flow
    | Error err ->
      Log.err (fun m -> m "Error while reading a line: %a" pp_error err) ;
      Fiber.return None
