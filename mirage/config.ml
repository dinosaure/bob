open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"Set the port where the relay will listen."
      [ "p"; "port" ]
  in
  Key.(create "port" Arg.(opt int 9000 doc))

let secure_port =
  let doc =
    Key.Arg.info ~doc:"The port of the relay where secured rooms are available."
      [ "secure-port" ]
  in
  Key.(create "secure_port" Arg.(opt int 9001 doc))

let bob =
  foreign "Unikernel.Make"
    ~packages:
      [
        package "bob"
          ~pin:
            "git+https://github.com/dinosaure/bob.git#3c85fff2aba1bbf0d0e7f05427d7e41f9b7a7cc3";
        package "spoke" ~sublibs:[ "core" ]
          ~pin:
            "git+https://github.com/dinosaure/spoke.git#61f4e785d22d6002fd396862f75f515355197002";
        package "psq";
      ]
    ~keys:[ Key.v port; Key.v secure_port ]
    (time @-> stackv4v6 @-> job)

let time = default_time
let stackv4v6 = generic_stackv4v6 default_network
let () = register "bob" [ bob $ time $ stackv4v6 ]
