# Contributing to `bob`

Bob was done within the [robur.io][robur.io] group. A first contribution
without technical prerequisites is possible through the donation:
[here][donate]

If you want to contribute to the project, knowledge of OCaml is required. This
document mainly states the constraints that the development process must
respect. These constraints are based on several objectives:
- portability / accessibility (Linux, Mac, \*BSD and Windows)
- simplicity
- security

## Portability / Accessibility

Bob's portability is a central point of development. It is ensured by
compiling the software with the [Cosmopolitan][cosmopolitan] C library and
the _caml runtime_ compiled with it (see [Esperanto][esperanto]).

This constraint means that **all** C code must be compatible with Cosmopolitan.
In general, this is the case. However, it is **very** preferable to implement
something in OCaml than in C. So it's better to sacrifice speed to still
ensure portability. Bob's goal is not to be fast - but to be functional
everywhere.

This constraint also implies to be very careful with dependencies that can
bring C code (as is the case with [mirage-crypto][mirage-crypto]). Once again,
portability remains one of the central objectives. This is why there is
[Fiber][./lib/fiber.mli] (a re-implementation of [lwt][lwt]) which brings in
very few C implementations.

It is important to understand that there are 2 compilation paths:
- the usual path (`dune build`)
- the path with Cosmopolitan (systematically tested with GitHub actions)

Both must always work! The first one is the easier one. The second one is a
static build: thus, both Bob and its dependencies (as well as transitive
dependencies) must compile with Cosmopolitan. It may happen that the first one
works while the second one fails. In this case, it is certainly because of a
(transitive) dependency.

### Build and test `bob`

You can easily build and test `bob` via OPAM:
```sh
$ git clone https://github.com/dinosaure/bob
$ cd bob
$ opam pin add -yn .
$ opam install --deps-only bob
$ dune build
$ dune runtest
```

To test the Cosmopolitan target, you must install [esperanto][esperanto] and
tweak a bit `dune` files:
```sh
$ opam install esperanto
$ cat >>bin/dune <<EOF
(rule
 (target bob.com)
 (enabled_if
  (= %{context_name} esperanto))
 (mode promote)
 (deps bob.exe)
 (action
  (run objcopy -S -O binary %{deps} %{target})))
EOF
$ cat >>dune-workspace <<EOF
(context
 (default
  (name esperanto)
  (toolchain esperanto)
  (merlin)
  (host default)))
$ dune build ./bin/bob.com
```

For more details about Cosmopolitan & Esperanto, you should see the repository
as well as the Cosmopolitan website.

## Simplicity

The definition of simplicity can be very subjective as to Bob's usage. In this,
we will clarify this point. What is meant by simplicity is not necessarily a
use whose prerequisite for use is based on a social construction of what
software should be. Such a prerequisite is not only determined (uses change)
but it is certainly not complete.

It would be _usual_ for software such as Bob to offer a GUI for example in
order to "facilitate" the user experience. Not because the GUI is simple but
because it is a socially "common" representation for any user. And it is
therefore through this common knowledge that the user can apprehend such
software "in a simple way".

We consider that such a view of simplicity is fundamentally wrong since it is
not based on the notion of simplicity (in the radical sense of the term) but
on a socially common concept that makes this use "seem" simple.

We therefore have a more radical view of simplicity that ultimately offers no
credit to what can be done in terms of UI/UX today, tomorrow or yesterday. Our
simplicity is based on a usage that may (and should) require new knowledge but
which above all ensures a (re)appropriation of the usage by the user.

In other words, to really simplify the use of software, one must understand and
learn how to use the software. These termes are not antinomic, even if a notion
of learning and understanding is at work, which could be nonsense, and that is
our notion of simplicity.

Our objective is not to focus on a particular use of Bob and to dismantle each
barrier so that the user can access a single use as simply as possible, but to
consider several use cases that Bob can manage through a spontaneity of uses
that the user should make his own.

## Security

Bob relies mainly on [Spoke][spoke] for the handshake and mirage-crypto for the
transmission of the document. For the former library, the implementation of the
calculations comes from [`libsodium`][libsodium] - the required primitives are
not available with `mirage-crypto`.

If a security hole exists in the handshake or transfer, it is best to create an
issue on these repositories.

More specifically, the security aspect of bob must be **systematic**. That is
to say that this question must be asked at all levels of interaction between
the software and the outside world:
- when it comes to resolving a domain name
- when initiating the _handshake_
- when it comes to transferring the document

In this, we prioritise a connection to a DNS resolver via DoT for example. The
"default values" (relay, name resolver, etc.) are chosen according to high
criteria.
- The DNS resolver is [unicast.censurfridns.dk][censurfridns.dk] with TLS
- The relay is administered by yours truly ([osau.re][osau.re])

Security also means that the user can choose (and thus have full control) over
all interactions that bob can have. Implementing a new feature therefore
implies that it is transparent at all levels up to the end user.

## How to contribute?

Like any project on GitHub, you can participate in development through
pull-requests and issues. For the first method, an addition of code must pass
the CI:
- the code must be formatted correctly (you can reformat your code with
  `dune build @fmt --auto`)
- the code must compile (with or without Cosmopolitan)

We don't expect a special format in the issues or in the pull-requests but just
politeness and kindness. Finally, even if you do not receive a response,
someone somewhere has surely seen your contribution and will participate in due
course. So don't expect the maintainers to pay particular attention to your
contribution, it will happen sooner or later.

[robur.io]: https://robur.io/
[donate]: https://robur.io/Donate
[osau.re]: https://din.osau.re/
[censurfridns.dk]: https://blog.uncensoreddns.org/
[libsodium]: https://github.com/jedisct1/libsodium
[cosmopolitan]: https://github.com/jart/cosmopolitan
[esperanto]: https://github.com/dinosaure/esperanto
[mirage-crypto]: https://github.com/mirage/mirage-crypto
[lwt]: https://github.com/ocsigen/lwt
[spoke]: https://github.com/dinosaure/spoke
