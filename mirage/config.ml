(* mirage >= 4.6.0 & < 4.6.1 *)

open Mirage

let setup = runtime_arg ~pos:__POS__ "Unikernel.K.setup"

let bob =
  main "Unikernel.Make" ~runtime_args:[ setup ]
    ~packages:
      [
        package "bob" ~pin:"git+https://github.com/dinosaure/bob.git";
        package "spoke" ~sublibs:[ "core" ];
        package "psq";
      ]
    (time @-> stackv4v6 @-> job)

let time = default_time
let stackv4v6 = generic_stackv4v6 default_network
let () = register "bob" [ bob $ time $ stackv4v6 ]
