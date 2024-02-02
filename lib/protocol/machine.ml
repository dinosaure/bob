type error = [ `Version_mismatch | `Unknown_version of int | `Invalid_machine ]

let pp_error ppf = function
  | `Version_mismatch -> Fmt.string ppf "Version mismatch"
  | `Unknown_version n -> Fmt.pf ppf "Unknown version %d" n
  | `Invalid_machine -> Fmt.string ppf "Unexpected state transition"

(* NOTE(dinosaure): historically, the sender talks first to the receiver. To
   catch a possible error between both, the best is to let the receiver to
   receive the first gently [hello]. So Bob should speak first. À bas le
   patriarcat! *)

let bob ?metadata ctx =
  let open Protocol in
  let bob's_capabilities = [ `Version 1; `Resume; `Size ] in
  let* () = send ctx ping () in
  let* () = recv ctx pong in
  let* () = send ctx hello bob's_capabilities in
  let* alice's_capabilities = recv ctx hello in
  let capabilities =
    Capability.common bob's_capabilities alice's_capabilities
  in
  match Capability.get capabilities Capability.Version with
  | Some (`Version 1) -> (
      let* () =
        match metadata with
        | Some metadata ->
            let* () =
              Option.map
                (fun v -> send ctx size v)
                (Metadata.size_of_document metadata)
              |> Option.value ~default:(return ())
            in
            let* () = send ctx kind (Metadata.kind_of_document metadata) in
            let* () = send ctx ping () in
            recv ctx pong
        | None ->
            let* () = send ctx ping () in
            recv ctx pong
      in
      let* packet = recv ctx any in
      match packet with
      | Any (Resume, (hash, cursor)) ->
          let* () = recv ctx ready in
          return (`Ready_to_resume (hash, cursor))
      | Any (Ready, ()) -> return `Ready_to_send
      | Any (Quit, ()) -> return `Quit
      | Any _ ->
          let* () = send ctx quit () in
          return `Quit)
  | Some (`Version n) ->
      let* () = recv ctx quit in
      fail (`Unknown_version n)
  | None ->
      Logs.warn (fun m -> m "No common version between you and your peer");
      let* () = recv ctx quit in
      fail `Version_mismatch

type metadata = [ `Size of int64 | `Kind of [ `File | `Files | `Directory ] ]

let metadata ~version values =
  match
    ( List.find_opt (function `Size _ -> true | _ -> false) values,
      List.find_opt (function `Kind _ -> true | _ -> false) values,
      version )
  with
  | Some (`Size size), Some (`Kind kind), `Version 1 ->
      Some (Metadata.v1 ~size kind)
  | None, Some (`Kind kind), `Version 1 -> Some (Metadata.v1 kind)
  | _ -> None

let alice ?resume:r ctx =
  let open Protocol in
  let alice's_capabilities = [ `Version 1; `Resume; `Size ] in
  let* () = recv ctx ping in
  let* () = send ctx pong () in
  let* bob's_capabilities = recv ctx hello in
  let* () = send ctx hello alice's_capabilities in
  let capabilities =
    Capability.common bob's_capabilities alice's_capabilities
  in
  match Capability.get capabilities Capability.Version with
  | Some (`Version 1) ->
      let rec collect (vs : metadata list) =
        let* value = recv ctx any in
        match value with
        | Any (Size, n) -> collect (`Size n :: vs)
        | Any (Kind, k) -> collect (`Kind k :: vs)
        | Any (Ping, _) ->
            let* () = send ctx pong () in
            return vs
        | _ ->
            let* () = send ctx quit () in
            fail `Invalid_machine
      in
      let* values = collect [] in
      let metadata = metadata ~version:(`Version 1) values in
      let* () =
        match r with
        | Some r ->
            let hash, cursor = Resume.current r in
            send ctx resume (hash, cursor)
        | None -> return ()
      in
      let* () = send ctx ready () in
      return (`Ready_to_recv metadata)
  | Some (`Version n) ->
      let* () = recv ctx quit in
      fail (`Unknown_version n)
  | None ->
      Logs.warn (fun m -> m "No common version between you and your peer");
      let* () = recv ctx quit in
      fail `Version_mismatch
