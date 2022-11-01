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
        package "bob" ~pin:"https://github.com/dinosaure/bob.git";
        package "spoke" ~sublibs:[ "core" ];
        package "psq";
      ]
    ~keys:[ Key.v port; Key.v secure_port ]
    (time @-> stackv4v6 @-> job)

let time = default_time
let stackv4v6 = generic_stackv4v6 default_network
let () = register "bob" [ bob $ time $ stackv4v6 ]
