module SHA256 = struct
  include Digestif.SHA256

  let compare a b = Eqaf.compare_le (to_raw_string a) (to_raw_string b)
end

type t =
  | V1 of { cur : SHA256.t * int64; res : SHA256.t * int64; tmp : Fpath.t }

let compare a b =
  match (a, b) with
  | V1 { cur; res; tmp }, V1 { cur = cur'; res = res'; tmp = tmp' } ->
      let tmp_v = Fpath.compare tmp tmp'
      and cur_v = SHA256.compare (fst cur) (fst cur')
      and res_v = SHA256.compare (fst res) (fst res') in
      if tmp_v = 0 && cur_v = 0 && res_v = 0 then
        Int64.compare (snd cur) (snd cur') - Int64.compare (snd res) (snd res')
      else List.find (( <> ) 0) [ tmp_v; cur_v; res_v ]

let pp ppf = function
  | V1 { cur; res; tmp } ->
      Fmt.pf ppf
        "@[<1>(V1@ @[<hov>{ cur= @[<1>(%a,@ %Ld)@];@ res= @[<1>(%a,@ %Ld)@];@ \
         tmp= %a; }@])@]"
        SHA256.pp (fst cur) (snd cur) SHA256.pp (fst res) (snd res) Fpath.pp tmp

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let v1 ~cur ~res tmp = V1 { cur; res; tmp }
let current = function V1 { cur; _ } -> cur

let of_json ic =
  let ( >>= ) = Result.bind in
  Json.of_json ic >>= function
  | `O
      [
        ("cur", `A [ `String cur; `Float n ]);
        ("res", `A [ `String res; `Float m ]);
        ("tmp", `String tmp);
        ("ver", `Float 1.);
      ] -> (
      try
        let cur = SHA256.of_hex cur in
        let res = SHA256.of_hex res in
        let tmp = Fpath.v tmp in
        Ok
          (V1
             {
               cur = (cur, Int64.of_float n);
               res = (res, Int64.of_float m);
               tmp;
             })
      with _ -> error_msgf "Invalid resume (v1) file")
  | _ -> error_msgf "Invalid resume file"

let to_json oc = function
  | V1 { cur; res; tmp } ->
      let json =
        `O
          [
            ( "cur",
              `A
                [
                  `String (SHA256.to_hex (fst cur));
                  `Float (Int64.to_float (snd cur));
                ] );
            ( "res",
              `A
                [
                  `String (SHA256.to_hex (fst res));
                  `Float (Int64.to_float (snd res));
                ] );
            ("tmp", `String (Fpath.to_string tmp));
            ("ver", `Float 1.);
          ]
      in
      Json.to_json oc json
