let src = Logs.Src.create "bob.crypto"

module Log = (val Logs.src_log src : Logs.LOG)

module type CIPHER_BLOCK = sig
  type key

  val authenticate_encrypt :
    key:key -> nonce:string -> ?adata:string -> string -> string

  val authenticate_decrypt :
    key:key -> nonce:string -> ?adata:string -> string -> string option

  val of_secret : string -> key
  val tag_size : int
end

type 'k cipher_block = (module CIPHER_BLOCK with type key = 'k)

let module_of : type k. k Spoke.aead -> k cipher_block = function
  | Spoke.GCM -> (module Mirage_crypto.AES.GCM)
  | Spoke.CCM16 -> (module Mirage_crypto.AES.CCM16)
  | Spoke.ChaCha20_Poly1305 ->
      let module M = struct
        include Mirage_crypto.Chacha20

        let tag_size = Mirage_crypto.Poly1305.mac_size
      end in
      (module M)

type symmetric =
  | Symmetric : {
      key : 'k;
      nonce : string;
      impl : 'k cipher_block;
    }
      -> symmetric

external xor_into_bytes :
  string -> src_off:int -> bytes -> dst_off:int -> len:int -> unit
  = "mc_xor_into_bytes"
[@@noalloc]

let xor src dst =
  let len = Int.min (String.length src) (Bytes.length dst) in
  xor_into_bytes src ~src_off:0 dst ~dst_off:0 ~len

let make_nonce nonce seq =
  let len = String.length nonce in
  let buf = Bytes.make (len + 8) '\000' in
  Bytes.set_int64_be buf len seq;
  xor nonce buf;
  Bytes.unsafe_to_string buf

let make_adata len =
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 Spoke.version;
  Bytes.set_uint16_be buf 2 len;
  Bytes.unsafe_to_string buf

let encrypt (Symmetric { key; nonce; impl = (module Cipher_block) }) seq str =
  let nonce = make_nonce nonce seq in
  let adata = make_adata (String.length str) in
  Cipher_block.authenticate_encrypt ~key ~adata ~nonce str

let decrypt (Symmetric { key; nonce; impl = (module Cipher_block) }) seq str =
  let nonce = make_nonce nonce seq in
  let adata = make_adata (String.length str - Cipher_block.tag_size) in
  Cipher_block.authenticate_decrypt ~key ~adata ~nonce str

type 'fd t = {
  fd : 'fd;
  recv : symmetric;
  send : symmetric;
  recv_record : Cstruct.t;
  send_record : bytes;
  mutable recv_seq : int64;
  mutable send_seq : int64;
  recv_queue : Qe.t;
  send_queue : Qe.t;
  mutable closed : bool;
}

let symmetric_of_key_nonce_and_cipher key_nonce (Spoke.AEAD aead) =
  let key_len = 32 in
  let nonce_len = 12 in
  let module Cipher_block = (val module_of aead) in
  let key = String.sub key_nonce 0 key_len in
  let key = Cipher_block.of_secret key in
  let nonce = String.sub key_nonce key_len nonce_len in
  Symmetric { key; nonce; impl = (module Cipher_block) }

let max_packet = 0x7fff

let make ~ciphers:(cipher0, cipher1) ~shared_keys:(k0, k1) fd =
  let recv = symmetric_of_key_nonce_and_cipher k0 cipher0 in
  let send = symmetric_of_key_nonce_and_cipher k1 cipher1 in
  let recv_queue =
    let (Symmetric { impl = (module Cipher_block); _ }) = recv in
    Qe.create (2 + max_packet + Cipher_block.tag_size)
  in
  let send_queue = Qe.create max_packet in
  let recv_record =
    let (Symmetric { impl = (module Cipher_block); _ }) = recv in
    Cstruct.create (2 + max_packet + Cipher_block.tag_size)
  in
  let send_record =
    let (Symmetric { impl = (module Cipher_block); _ }) = recv in
    Bytes.make (2 + max_packet + Cipher_block.tag_size) '\000'
  in
  {
    fd;
    recv;
    send;
    recv_record;
    send_record;
    recv_seq = 0L;
    send_seq = 0L;
    recv_queue;
    send_queue;
    closed = false;
  }

let get_record record queue symmetric =
  let (Symmetric { impl = (module Cipher_block); _ }) = symmetric in
  match Qe.size queue with
  | 0 | 1 -> `Await_hdr
  | 2 | _ -> (
      let { Cstruct.buffer = record_buffer; off = record_off; _ } = record in
      Qe.keep queue ~off:record_off ~len:2 record_buffer;
      let packet = Cstruct.BE.get_uint16 record 0 in
      match packet with
      | 0xffff -> `Close
      | len ->
          let required = 2 + len + Cipher_block.tag_size in
          if Qe.size queue >= required then (
            Qe.keep queue ~off:record_off ~len:required record_buffer;
            Qe.shift queue required;
            let off = 2 and len = len + Cipher_block.tag_size in
            let str = Cstruct.to_string ~off ~len record in
            `Record str)
          else `Await_rec)

let record ~dst ~seq queue symmetric =
  let len = min max_packet (Qe.size queue) in
  Qe.keep_bytes queue ~off:2 ~len dst;
  let str = encrypt symmetric seq (Bytes.sub_string dst 2 len) in
  Qe.shift queue len;
  Bytes.set_uint16_be dst 0 len;
  Bytes.blit_string str 0 dst 2 (String.length str);
  Bytes.sub_string dst 0 (2 + String.length str)

module type FLOW = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type flow
  type error
  type write_error = private [> `Closed ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
  val read : flow -> ([ `Data of string | `Eof ], error) result t
  val write : flow -> string -> (unit, write_error) result t
  val close : flow -> unit t
end

module Make (Flow : FLOW) = struct
  type error = [ `Rd of Flow.error | `Corrupted ]

  let pp_error ppf = function
    | `Rd err -> Fmt.pf ppf "read(): %a" Flow.pp_error err
    | `Corrupted -> Fmt.pf ppf "Encrypted data corrupted"

  type write_error = [ `Closed | `Wr of Flow.write_error ]

  let pp_write_error ppf = function
    | `Closed | `Wr `Closed -> Fmt.pf ppf "Connection closed by peer"
    | `Wr err -> Fmt.pf ppf "write(): %a" Flow.pp_write_error err

  let ( >>= ) = Flow.bind

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error _ as err -> Flow.return err

  let ( >>| ) x f = x >>= fun x -> Flow.return (f x)
  let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)
  let close_packet = "\xff\xff\xff\xff"

  let send_close flow =
    Flow.write flow.fd close_packet >>| reword_error (fun err -> `Wr err)

  let rec recv flow =
    if flow.closed then Flow.return (Ok `End) else go flow.recv_record flow

  and go recv_record flow =
    match get_record recv_record flow.recv_queue flow.recv with
    | `Close ->
        flow.closed <- true;
        send_close flow >>= fun res ->
        (match res with
        | Error err ->
            Log.err (fun m ->
                m "Impossible to notify peer to close: %a" pp_write_error err)
        | Ok () -> ());
        Flow.return (Ok `End)
    | `Record str -> (
        match decrypt flow.recv flow.recv_seq str with
        | Some str ->
            flow.recv_seq <- Int64.succ flow.recv_seq;
            Flow.return (Ok (`Data str))
        | None ->
            Log.err (fun m -> m "Impossible to decrypt the record.");
            Flow.return (Error `Corrupted))
    | (`Await_hdr | `Await_rec) as await -> (
        Flow.read flow.fd >>= function
        | Error err -> Flow.return (Error (`Rd err))
        | Ok `Eof ->
            if await = `Await_hdr then Flow.return (Ok `End)
            else Flow.return (Error `Corrupted)
        | Ok (`Data str) ->
            Qe.push_string flow.recv_queue str;
            go recv_record flow)

  let rec flush flow =
    if not (Qe.is_empty flow.send_queue) then (
      let record =
        record ~dst:flow.send_record ~seq:flow.send_seq flow.send_queue
          flow.send
      in
      flow.send_seq <- Int64.succ flow.send_seq;
      Flow.write flow.fd record >>| reword_error (fun err -> `Wr err)
      >>? fun () -> flush flow)
    else Flow.return (Ok ())

  let send flow str ~off ~len =
    if flow.closed then Flow.return (Error `Closed)
    else (
      Qe.push_string flow.send_queue (String.sub str off len);
      flush flow >>? fun () -> Flow.return (Ok len))

  let close flow =
    (flush flow >>? fun () -> send_close flow) >>= fun res ->
    (match res with
    | Error err ->
        Log.err (fun m ->
            m "Impossible to flush and/or notify peer to close: %a"
              pp_write_error err)
    | Ok () -> ());
    flow.closed <- true;
    (* TODO(dinosaure): should we close the underlying flow? *)
    Flow.return ()
end
