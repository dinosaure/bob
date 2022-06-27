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
server$ dune exec bin/bob.exe -- send <password>
client$ dune exec bin/bob.exe -- recv <password>
Accept from <server-identity> [Y/n]: Y
Handshake is done with <server-identity>
```

### Avantage of `bob`

#### The relay implementation

One of the advantage of `bob` is the implementation of its relay, which simply
transfers information from one peer to another without altering the content.
The investigators of the agreement are **only** the peers and the relay does
not intervene **in any way** in this agreement. The sole role of the relay is
to transfer information from one peer to another. When two peers reach an
agreement, they notif the relay so that it can allocate a secure channel
between the two peers.

The relay is therefore _blind_ to the algorithm used to reach an agreement.
This feature ensures that there is no compromise between peers via the relay.

#### OCaml & GADTs

The state machine defined to ensure the exchange uses an aspect of the OCaml
language: GADT. From this we can encore at type level that a client cannot
talk to another client and a server cannot talk to another server. In this way,
we can prune problematic cases as errors upstream, outside the implementation
of the so-called state machine.

[spoke]: https://github.com/dinosaure/spoke
[article]: https://blog.osau.re/articles/spoke.html
