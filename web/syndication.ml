module Person = struct
  let romain_calascibetta =
    Syndic.Atom.author
      ~uri:(Uri.of_string "https://blog.osau.re/")
      ~email:"romain.calascibetta@gmail.com" "Romain Calascibetta"
end

module Category = struct
  let general = Syndic.Atom.category "general"
  let bugfix = Syndic.Atom.category "bugfix"
  let relay = Syndic.Atom.category "relay"
end

let generate_id g =
  let v = Uuidm.v4_gen g () in
  Uri.of_string (Fmt.str "urn:uuid:%s" (Uuidm.to_string v))

let of_markdown str =
  let doc = Cmarkit.Doc.of_string str in
  Syndic.Atom.Html (None, Cmarkit_html.of_doc ~safe:true doc)

let entry_2022_12_14_0 g =
  let id = generate_id g in
  let updated = Syndic.Date.of_rfc3339 "2022-12-14 14:00:00+01:00" in
  let content =
    of_markdown
      {markdown|Creation of an ATOM feed for B·o·B

The website https://bob.osau.re/ has an ATOM feed: https://bob.osau.re/feed.xml.
The idea is to propose a way to notice users about improvements/updates on our
relay and about our software.

The purpose of this feed is:
- notice the user when we shutdown/restart the relay
- when we change the version of our relay
- when we did a huge improvement on Bob
- the status of the project more generally

The feed is generated automatically via Syndic (see
https://github.com/Cumulus/Syndic) and available into our main GitHub
repository (see https://github.com/dinosaure/bob/tree/main/web).|markdown}
  in
  Syndic.Atom.entry
    ~authors:Person.(romain_calascibetta, [])
    ~title:(Text "Creation of an ATOM feed for B·o·B") ~updated
    ~published:updated ~content
    ~categories:Category.[ general ]
    ~id ()

let entry_2022_12_14_1 g =
  let id = generate_id g in
  let updated = Syndic.Date.of_rfc3339 "2022-12-14 15:00:00+01:00" in
  let content =
    of_markdown
      {markdown|Bugfix on MacOS/FreeBSD

Fabien Dagnat spotted a bug on MacOS and (transitively) on FreeBSD. The problem
was when we wanted to send a file from this machine. The receiver received a
corrupted file. The issue is related to the underlying TCP/IP stack of the
system. On MacOS/FreeBSD, the write() syscall does not ensure that everything
was sent but Bob assumed that it was the case. The fix can be found by the
commit f7c404d7066492a0e1445d9941143e8181685cb6.

From this issue, we made several improvements:
- we implemented a CLOSE packet to be sure that the connection is not closed
  until the receiver received everything (like CLOSE_NOTIFY for TLS or QUIT
  for SMTP)
- we added several checks (bounds checking) and internalize our implementation
  of a ring-buffer (instead of our usage of the ke library)
- we introduced the new option `--reproduce` which permits to debug the Bob's
  behavior by a reproductible way to construct the private shared key
- we fixed our usage of `select()` on MacOS
- we decided to ignore the SIGPIPE signal (on the sender side)
- we deleted some dead-code
- we decided to use `getprotobyname()` instead of an arbitrary value when we
  create a new `socket()`
- and some minor improvements

We would like to thank Fabien Dagnat for his patience, Kate (@kit-ty-kate) for
kindly giving us access to a Mac and the Robur team.

The relay was upgraded and our reproducible infrastructure will (soonly) build
the new version of bob.com and bob.hvt which include all of these improvements.
Old versions of Bob should work but we advise to upgrade your binary with the
new one available here:

https://builds.osau.re/job/bob/build/latest/f/bin/bob.com|markdown}
  in
  Syndic.Atom.entry
    ~authors:Person.(romain_calascibetta, [])
    ~title:(Text "Bugfix on MacOS/FreeBSD") ~updated ~published:updated ~content
    ~categories:Category.[ bugfix; relay ]
    ~id ()

let entry_2023_01_18 g =
  let id = generate_id g in
  let updated = Syndic.Date.of_rfc3339 "2023-01-18 14:00:00+01:00" in
  let content =
    of_markdown
      {markdown|New version of Esperanto (0.0.2)

I finally took the time to update Esperanto to finally use Cosmopolitan v2.2.
This version of Esperanto produces the `threads.cmxa` required for libraries
like `lwt`. As far as Bob is concerned, this should not change anything in its
behaviour but this version change integrates the various bugs fixed by Justine
(Cosmopolitan's author) as well as the support of OCaml 4.14.1.

The link to download Bob on the website does not ensure reproducibility with
previous versions. However, there should be no problem with the compatibility
between the old versions of Bob and the new one.|markdown}
  in
  Syndic.Atom.entry
    ~authors:Person.(romain_calascibetta, [])
    ~title:(Text "Bob compiled with a new version of Esperanto (0.0.2)")
    ~updated ~published:updated ~content ~id ()

let entry_2023_28_04 g =
  let id = generate_id g in
  let updated = Syndic.Date.of_rfc3339 "2023-04-28 14:00:00+01:00" in
  let content =
    of_markdown
      {markdown|Esperanto 0.0.3 and portability

The latest version of Esperanto includes a major new change regarding Bob's
portability. It is now possible to run Bob on arm64/aarch64 computers.
Cosmopolitan now statically integrates a tool, blink, which allows to virtualise
the executable on such platforms "à la qemu". This means that Bob is now
**really** portable to all platforms.

The website will be updated accordingly.|markdown}
  in
  Syndic.Atom.entry
    ~authors:Person.(romain_calascibetta, [])
    ~title:(Text "Esperanto 0.0.3 and portability") ~updated ~published:updated
    ~content ~id ()

let entry_2023_31_12 g =
  let id = generate_id g in
  let updated = Syndic.Date.of_rfc3339 "2023-12-31 12:00:00+01:00" in
  let content =
    of_markdown
      {markdown|Esperanto.0.0.5 and portability

Hello Bob users. On this festive day, a major update has just been made to Bob.
We have just updated Cosmopolitan to version 3.1 (released in November 2023).
Among other things, this makes Bob available natively on aarch64 architectures
(previous versions used blink to translate the x86_64 assembler to aarch64).
We're delighted that Bob now runs on Macs with an Apple Silicon processor.

For 2024, we now have clear objectives:
- support for Tor and anonymisation (already functional but requiring a change to the protocol)
- support for 'resuming' a failed transfer
- transfer via UDP on a local network|markdown}
  in
  Syndic.Atom.entry
    ~authors:Person.(romain_calascibetta, [])
    ~title:(Text "Esperanto.0.0.5 and portability") ~updated ~published:updated
    ~content ~id ()

let generator = Syndic.Atom.generator ~version:"1.6.1" "Syndic"

let feed g =
  let links =
    [
      Syndic.Atom.link ~rel:Self (Uri.of_string "https://bob.osau.re/feed.xml");
      Syndic.Atom.link (Uri.of_string "https://bob.osau.re/");
    ]
  in
  let entries =
    List.map
      (fun entry -> entry g)
      [
        entry_2022_12_14_0;
        entry_2022_12_14_1;
        entry_2023_01_18;
        entry_2023_28_04;
        entry_2023_31_12;
      ]
  in
  let authors =
    List.fold_left
      (fun acc ({ Syndic.Atom.authors = x, r; _ } : Syndic.Atom.entry) ->
        let acc = if List.exists (( = ) x) acc then acc else x :: acc in
        List.fold_left
          (fun acc author ->
            if List.exists (( = ) author) acc then acc else author :: acc)
          acc r)
      [] entries
  in
  let updated =
    List.fold_left
      (fun acc ({ Syndic.Atom.updated; _ } : Syndic.Atom.entry) ->
        if Syndic.Date.compare acc updated < 0 then updated else acc)
      Syndic.Date.epoch entries
  in
  let id = generate_id g in
  let title =
    let of_markdown str : Syndic.Atom.title =
      let doc = Cmarkit.Doc.of_string str in
      Syndic.Atom.Html (None, Cmarkit_html.of_doc ~safe:true doc)
    in
    of_markdown
      {markdown|"B·o·B, an universal & secure peer-to-peer file-transfer in OCaml"|markdown}
  in
  Syndic.Atom.feed ~title ~generator ~authors ~id ~updated ~links entries

let to_int_array str =
  let res = Array.make (String.length str / 2) 0 in
  for i = 0 to (String.length str / 2) - 1 do
    res.(i) <- (Char.code str.[i * 2] lsl 8) lor Char.code str.[(i * 2) + 1]
  done;
  res

let ( <.> ) f g x = f (g x)
let ( & ) f x = f x

let () =
  match Sys.argv with
  | [| _; seed |] -> (
      match
        Result.map (Random.State.make <.> to_int_array) (Base64.decode seed)
      with
      | Ok g ->
          let feed = feed g in
          Syndic.Atom.output feed (`Channel stdout)
      | Error (`Msg err) -> invalid_arg err)
  | _ ->
      let g =
        Random.State.make & to_int_array
        & Base64.decode_exn "8+tje7wq4eideiPe122KpPVHcOxx51iXJZgzzYSBWTk="
      in
      let feed = feed g in
      Syndic.Atom.output feed (`Channel stdout)
