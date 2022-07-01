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
$ bob relay
$ bob send
Password: giato-fextre

$ bob giato-fextre
Accept from <server-identity> [Y/n]: Y
Handshake is done with <server-identity>
```

The current state allow you to talk to with your peer through the relay. Of
course, the communication is encrypted and the relay is not able to decrypt
anything. Only peers have shared keys. That mostly means that from a shared
weak password, we are able to initiate a truly secured communication with
a peer. The next step will be file transferring.

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

#### Unikernels and [MirageOS][mirage]

Bob provides 2 implementations of the relay which are very similar. One is a
part of the executable and the user can launch its own relay _via_ `bob relay`
and an other implementation exists as an unikernel (see [mirage/][./mirage]).

The last one lets the user to compile a full operating system as a relay and
virtualize it with KVM for instance. You must have a machine with [KVM][kvm].
You must install the `mirage` tool and install `bob` first. Then, you should be
able to craft the operating system with:
```sh
$ opam install mirage
$ git clone https://github.com/dinosaure/bob
$ mkdir bob-unikernel
$ cp bob/mirage/* bob-unikernel/
$ cd bob-unikernel/
$ mirage configure -t hvt
$ make depends
$ mirage build
$ ls dist/bob.hvt
dist/bob.hvt
```

An image `bob.hvt` is crafted and it can be launched with [Solo5][solo5] and
[albatross][albatross]. Albatross is available _via_ `apt` if you want:
```sh
$ wget -q -O - https://apt.robur.coop/gpg.pub | apt-key add -
$ echo "deb https://apt.robur.coop ubuntu-20.04 main" >> /etc/apt/sources.list
$ sudo apt update
$ sudo apt install solo5-hvt albatross
```

##### Networks and unikernels

For OS virtualization, you usually requires a bridge:
```
$ cat >>/etc/network/interfaces <<EOF

auto service
iface service inet static
  address 10.0.0.1
  netmask 255.255.255.0
  broadcast 10.0.0.255
  bridge_ports none
  bridge_stp off
  bridge_fd 0
  bridge_maxwait 0
EOF
$ systemctl restart networking
```

Finally, you need to let the unikernel to communicate with Internet and let
people to communicate with your unikernel:
```sh
$ cat "1" > /proc/sys/net/ipv4/ip_forward
$ iptables -A FORWARD -o service -m conntrack --ctstate RELATED,ESTABLISHED \
    -j ACCEPT
$ iptables -A FORWARD -i service ! -o service -j ACCEPT
$ iptables -A FORWARD -i service -o service -j ACCEPT
$ iptables -t nat -A POSTROUTING -s 10.0.0.0/24 ! -o service \
    -j MASQUERADE
$ iptables -N BOB
$ iptables -A BOB -d 10.0.0.2/32 ! -i service -o service \
    -p tcp -m tcp --dport 9000 -j ACCEPT
$ iptables -A BOB -d 10.0.0.2/32 ! -i service -o service \
    -p tcp -m tcp --dport 9001 -j ACCEPT
$ iptables -A FORWARD -o service -j BOB
$ iptables -t nat -N BOB
$ iptables -t nat -A PREROUTING -m addrtype --dst-type LOCAL -j BOB
$ iptables -t nat -A BOB ! -s 10.0.0.2/32 \
    -p tcp -m tcp --dport 9000 =j DNAT --to-destination 10.0.0.2:9000
$ iptables -t nat -A BOB ! -s 10.0.0.2/32 \
    -p tcp -m tcp --dport 9001 =j DNAT --to-destination 10.0.0.2:9000
```

##### Launch the unikernel

You can launch the unikernel with `albatross` with:
```sh
$ albatross-client-local create --net=service \
    --arg="--ipv4=10.0.0.2/24"
    --arg="--ipv4-gateway=10.0.0.1" bob bob.hvt
```

Locally, you are able to communicate with your relay _via_ the `-r` option:
```sh
$ bob send -r <my-public-ip>:9000
Password: shoacquis-feursonsindlebu

$ bob -r <my-public-ip>:9000 shoacquis-feursonsindlebu
Accept from <server-identity> [Y/n]: Y
Handshake is done with <server-identity>
```

#### [Esperanto][esperanto], [Cosmopolitan][cosmopolitan] and Windows support

Currently, the `bob` executable can be compiled with the `esperanto` toolchain.
By this way, we are able to delier a `bob.com` which works _anywhere_. The
status of it is experimental. However, few tweak on some libraries (specially
`mirage-crypto` and `digestif`) are needed to be able to compile `bob`
with this _toolchain_.

The final executable, the `bob.com` seems to work on PowerShell (Windows) and
obviously Linux.

[spoke]: https://github.com/dinosaure/spoke
[article]: https://blog.osau.re/articles/spoke.html
[mirage]: https://mirage.io/
[kvm]: https://en.wikipedia.org/wiki/Kernel-based_Virtual_Machine
[solo5]: https://github.com/Solo5/solo5
[albatross]: https://github.com/hannesm/albatross
[esperanto]: https://github.com/dinosaure/esperanto
[cosmopolitan]: https://github.com/jart/cosmopolitan
