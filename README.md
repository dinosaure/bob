<h1 align="center">B·o·B</h1>
<h4 align="center">A peer-to-peer file-transfer in OCaml</h4>
<hr>

**Work In Progress**: This repository just help me to save my work. The code
does not work and/or compile, but it's under an active development. You
probably should take a look on [spoke][spoke] and an [article][article] which
describe its implementation first. Then, enjoy to read GADTs and comments...

### A simple example

Currently, the distribution provides a simple binary which can be used as:
- a relay
- a peer (which one wants to send a file - the server - and which one wants to
  receive a file - the client).

You can install `bob` on your computer with:
```sh
$ opam pin add -y git+https://github.com/dinosaure/bob
```

Then, you can launch a relay, a server and a client:
```
$ git clone https://github.com/dinosaure/bob
$ cd bob
relay $ dune exec bin/bob.exe -- relay
client$ dune exec bin/bob.exe -- client <password>
server$ dune exec bin/bob.exe -- server <password>
```

[spoke]: https://github.com/dinosaure/spoke
[article]: https://blog.osau.re/articles/spoke.html
