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
  recv_queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
  send_queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
}

let symmetric_of_key_nonce_and_cipher key_nonce (Spoke.AEAD aead) =
  let key_len = 32 in
  let nonce_len = 12 in
  let module Cipher_block = (val module_of aead) in
  let key = Cstruct.of_string ~off:0 ~len:key_len key_nonce in
  let key = Cipher_block.of_secret key in
  let nonce = Cstruct.of_string ~off:key_len ~len:nonce_len key_nonce in
  Symmetric { key; nonce; impl = (module Cipher_block) }

let make ~ciphers:(cipher0, cipher1) ~shared_keys:(k0, k1) fd =
  let recv = symmetric_of_key_nonce_and_cipher k0 cipher0 in
  let send = symmetric_of_key_nonce_and_cipher k1 cipher1 in
  let recv_queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let send_queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let recv_record =
    let (Symmetric { impl = (module Cipher_block); _ }) = recv in
    Cstruct.create (2 + 0xffff + Cipher_block.tag_size)
  in
  let send_record =
    let (Symmetric { impl = (module Cipher_block); _ }) = send in
    Cstruct.create (2 + 0xffff + Cipher_block.tag_size)
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
  }

let get_record record queue symmetric =
  let (Symmetric { impl = (module Cipher_block); _ }) = symmetric in
  match Ke.Rke.length queue with
  | 0 -> `Await_hdr
  | 1 -> `Await_rec 1
  | 2 | _ ->
      let blit src src_off dst dst_off len =
        let src = Cstruct.of_bigarray src ~off:src_off ~len in
        Cstruct.blit src 0 dst dst_off len
      in
      Ke.Rke.N.keep_exn queue ~blit ~length:Cstruct.length record ~len:2;
      let len = Cstruct.BE.get_uint16 record 0 in
      let required = 2 + len + Cipher_block.tag_size in
      if Ke.Rke.length queue >= required then (
        Ke.Rke.N.keep_exn queue ~blit ~length:Cstruct.length record
          ~len:required;
        Ke.Rke.N.shift_exn queue required;
        `Record (Cstruct.sub record 2 (len + Cipher_block.tag_size)))
      else `Await_rec (len - Ke.Rke.length queue)

let record ~dst ~seq queue symmetric =
  let len = min 0xffff (Ke.Rke.length queue) in
  let blit src src_off dst dst_off len =
    let src = Cstruct.of_bigarray src ~off:src_off ~len in
    Cstruct.blit src 0 dst dst_off len
  in
  Ke.Rke.N.keep_exn queue ~length:Cstruct.length ~blit ~off:2 ~len dst;
  let buf = encrypt symmetric seq (Cstruct.sub dst 2 len) in
  Ke.Rke.N.shift_exn queue len;
  Cstruct.BE.set_uint16 dst 0 len;
  Cstruct.blit buf 0 dst 2 (Cstruct.length buf);
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

  let rec recv flow =
    match get_record flow.recv_record flow.recv_queue flow.recv with
    | `Record buf -> (
        match decrypt flow.recv flow.recv_seq buf with
        | Some buf ->
            flow.recv_seq <- Int64.succ flow.recv_seq;
            Flow.return (Ok (`Data (Cstruct.to_string buf)))
        | None -> Flow.return (Error `Corrupted))
    | (`Await_hdr | `Await_rec _) as await -> (
        Flow.read flow.fd >>= function
        | Error err -> Flow.return (Error (`Rd err))
        | Ok `Eof ->
            if await = `Await_hdr then Flow.return (Ok `End)
            else Flow.return (Error `Corrupted)
        | Ok (`Data cs) ->
            let blit src src_off dst dst_off len =
              let dst = Cstruct.of_bigarray dst ~off:dst_off ~len in
              Cstruct.blit src src_off dst 0 len
            in
            Ke.Rke.N.push flow.recv_queue ~blit ~length:Cstruct.length cs;
            recv flow)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error _ as err -> Flow.return err

  let ( >>| ) x f = x >>= fun x -> Flow.return (f x)
  let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

  let rec flush flow =
    if not (Ke.Rke.is_empty flow.send_queue) then (
      let record =
        record ~dst:flow.send_record ~seq:flow.send_seq flow.send_queue
          flow.send
      in
      flow.send_seq <- Int64.succ flow.send_seq;
      Flow.write flow.fd record >>| reword_error (fun err -> `Wr err)
      >>? fun () -> flush flow)
    else Flow.return (Ok ())

  let send flow str ~off ~len =
    let blit src src_off dst dst_off len =
      Stdbob.bigstring_blit_from_string src ~src_off dst ~dst_off ~len
    in
    Ke.Rke.N.push flow.send_queue ~blit ~length:String.length ~off ~len str;
    flush flow >>? fun () -> Flow.return (Ok len)

  let close { fd; _ } = Flow.close fd

  let rec getline queue flow =
    match Stdbob.line_of_queue queue with
    | Some line -> Flow.return (Some line)
    | None -> (
        recv flow >>= function
        | Ok `End -> Flow.return None
        | Ok (`Data str) ->
            let blit src src_off dst dst_off len =
              Stdbob.bigstring_blit_from_string src ~src_off dst ~dst_off ~len
            in
            Ke.Rke.N.push queue ~blit ~length:String.length str;
            getline queue flow
        | Error err ->
            Log.err (fun m -> m "Error while reading a line: %a" pp_error err);
            Flow.return None)
end
