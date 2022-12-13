let src = Logs.Src.create "bob.crypto"

module Log = (val Logs.src_log src : Logs.LOG)

module type CIPHER_BLOCK = sig
  type key

  val authenticate_encrypt :
    key:key -> nonce:Cstruct.t -> ?adata:Cstruct.t -> Cstruct.t -> Cstruct.t

  val authenticate_decrypt :
    key:key ->
    nonce:Cstruct.t ->
    ?adata:Cstruct.t ->
    Cstruct.t ->
    Cstruct.t option

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
      end in
      (module M)
  | Spoke.ChaCha20_Poly1305 ->
      let module M = struct
        include Mirage_crypto.Chacha20

        let tag_size = Mirage_crypto.Poly1305.mac_size
      end in
      (module M)

type symmetric =
  | Symmetric : {
      key : 'k;
      nonce : Cstruct.t;
      impl : 'k cipher_block;
    }
      -> symmetric

external xor_into :
  Cstruct.buffer ->
  src_off:int ->
  Cstruct.buffer ->
  dst_off:int ->
  len:int ->
  unit = "spoke_xor_into_generic_bigarray"

let xor src dst =
  let len = min (Cstruct.length src) (Cstruct.length dst) in
  xor_into (Cstruct.to_bigarray src) ~src_off:0 (Cstruct.to_bigarray dst)
    ~dst_off:0 ~len

let xor a b =
  let len = min (Cstruct.length a) (Cstruct.length b) in
  let res = Cstruct.of_string (Cstruct.copy b 0 len) in
  xor a res;
  res

let make_nonce nonce seq =
  let seq =
    let len = Cstruct.length nonce in
    let seq =
      let buf = Cstruct.create 8 in
      Cstruct.BE.set_uint64 buf 0 seq;
      buf
    in
    let pad = Cstruct.create (len - 8) in
    Cstruct.append pad seq
  in
  xor nonce seq

let make_adata len =
  let buf = Cstruct.create 4 in
  Cstruct.BE.set_uint16 buf 0 Spoke.version;
  Cstruct.BE.set_uint16 buf 2 len;
  buf

let encrypt (Symmetric { key; nonce; impl = (module Cipher_block) }) seq buf =
  let nonce = make_nonce nonce seq in
  let adata = make_adata (Cstruct.length buf) in
  Cipher_block.authenticate_encrypt ~key ~adata ~nonce buf

let decrypt (Symmetric { key; nonce; impl = (module Cipher_block) }) seq buf =
  let nonce = make_nonce nonce seq in
  let adata = make_adata (Cstruct.length buf - Cipher_block.tag_size) in
  Cipher_block.authenticate_decrypt ~key ~adata ~nonce buf

type 'fd t = {
  fd : 'fd;
  recv : symmetric;
  send : symmetric;
  recv_record : Cstruct.t;
  send_record : Cstruct.t;
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
  let key = Cstruct.of_string ~off:0 ~len:key_len key_nonce in
  let key = Cipher_block.of_secret key in
  let nonce = Cstruct.of_string ~off:key_len ~len:nonce_len key_nonce in
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
    let (Symmetric { impl = (module Cipher_block); _ }) = send in
    Cstruct.create max_packet
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
      Qe.keep queue record_buffer ~off:record_off ~len:2;
      let packet = Cstruct.BE.get_uint16 record 0 in
      match packet with
      | 0xffff -> `Close
      | len ->
          let required = 2 + len + Cipher_block.tag_size in
          if Qe.size queue >= required then (
            Qe.keep queue record_buffer ~off:record_off ~len:required;
            Qe.shift queue required;
            `Record (Cstruct.sub record 2 (len + Cipher_block.tag_size)))
          else `Await_rec)

let record ~dst ~seq queue symmetric =
  let len = min max_packet (Qe.size queue) in
  let { Cstruct.buffer = dst_buffer; off = dst_off; _ } = dst in
  Qe.keep queue ~off:(dst_off + 2) ~len dst_buffer;
  let buf = encrypt symmetric seq (Cstruct.sub dst 2 len) in
  Qe.shift queue len;
  Cstruct.BE.set_uint16 dst 0 len;
  Cstruct.blit buf 0 dst 2 len (* XXX(dinosaure): assert = Cstruct.length buf *);
  Cstruct.sub dst 0 (2 + Cstruct.length buf)

module type FLOW = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type flow
  type error
  type write_error = private [> `Closed ]

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
  val read : flow -> ([ `Data of Cstruct.t | `Eof ], error) result t
  val write : flow -> Cstruct.t -> (unit, write_error) result t
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
  let close_packet = Cstruct.of_string "\xff\xff\xff\xff"

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
    | `Record buf -> (
        match decrypt flow.recv flow.recv_seq buf with
        | Some buf ->
            flow.recv_seq <- Int64.succ flow.recv_seq;
            let { Cstruct.buffer; off; len } = buf in
            let res = Bigarray.Array1.sub buffer off len in
            Flow.return (Ok (`Data res))
        | None ->
            Log.err (fun m -> m "Impossible to decrypt the record.");
            Flow.return (Error `Corrupted))
    | (`Await_hdr | `Await_rec) as await -> (
        Flow.read flow.fd >>= function
        | Error err -> Flow.return (Error (`Rd err))
        | Ok `Eof ->
            if await = `Await_hdr then Flow.return (Ok `End)
            else Flow.return (Error `Corrupted)
        | Ok (`Data cs) ->
            let { Cstruct.buffer; off; len } = cs in
            Qe.push flow.recv_queue ~off ~len buffer;
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

  let send flow bstr ~off ~len =
    if flow.closed then Flow.return (Error `Closed)
    else (
      Qe.push flow.send_queue ~off ~len bstr;
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
    Flow.close flow.fd
end
