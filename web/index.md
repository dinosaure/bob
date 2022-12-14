<html lang="en" charset="utf-8">
<head>
  <title>B·o·B</title>
  <meta name="description" content="An universal & secure peer-to-peer file-transfer in OCaml" />
  <link rel="stylesheet" href="https://unpkg.com/terminal.css@0.7.2/dist/terminal.min.css" />
  <link rel="alternate" type="application/atom+xml" href="https//bob.osau.re/feed.xml" />
  <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/languages/bash.min.js"></script>
  <style>
.hljs {
  display: block;
  overflow-x: auto;
  padding: 0.5em;
  background: var(--block-background-color);
  color: var(--font-color);
}

.hljs-comment,
.hljs-quote {
  color: var(--secondary-color);
}

.hljs-variable {
    color: var(--font-color);
}

.hljs-keyword,
.hljs-selector-tag,
.hljs-built_in,
.hljs-name,
.hljs-tag {
  color: var(--primary-color);
}

.hljs-string,
.hljs-title,
.hljs-section,
.hljs-attribute,
.hljs-literal,
.hljs-template-tag,
.hljs-template-variable,
.hljs-type,
.hljs-addition {
  color: var(--secondary-color);
}

.hljs-string {
    color: var(--secondary-color);
}

.hljs-deletion,
.hljs-selector-attr,
.hljs-selector-pseudo,
.hljs-meta {
  color: var(--primary-color);
}

.hljs-doctag {
  color: var(--secondary-color);
}

.hljs-attr {
  color: var(--primary-color);
}

.hljs-symbol,
.hljs-bullet,
.hljs-link {
  color: var(--primary-color);
}


.hljs-emphasis {
  font-style: italic;
}

.hljs-strong {
  font-weight: bold;
}

[theme="dark"] {
       --global-font-size: 15px;
       --global-line-height: 1.4em;
       --global-space: 10px;
       --font-stack: Menlo, Monaco, Lucida Console, Liberation Mono,
         DejaVu Sans Mono, Bitstream Vera Sans Mono, Courier New, monospace,
         serif;
       --mono-font-stack: Menlo, Monaco, Lucida Console, Liberation Mono,
         DejaVu Sans Mono, Bitstream Vera Sans Mono, Courier New, monospace,
         serif;
       --background-color: #222225;
       --page-width: 60em;
       --font-color: #e8e9ed;
       --invert-font-color: #222225;
       --secondary-color: #a3abba;
       --tertiary-color: #a3abba;
       --primary-color: #62c4ff;
       --error-color: #ff3c74;
       --progress-bar-background: #3f3f44;
       --progress-bar-fill: #62c4ff;
       --code-bg-color: #3f3f44;
       --input-style: solid;
       --display-h1-decoration: none;
     }

/*
.theme-switch-wrapper {
  display: flex;
  align-items: center;
}
*/

.theme-switch {
  display: inline-block;
  height: 17px;
  position: relative;
  width: 30px;
}

.theme-switch input {
  display:none;
}

.slider {
  background-color: #ccc;
  bottom: 0;
  cursor: pointer;
  left: 0;
  position: absolute;
  right: 0;
  top: 0;
  transition: .4s;
}

.slider:before {
  background-color: #fff;
  bottom: 2px;
  content: "";
  height: 13px;
  left: 2px;
  position: absolute;
  transition: .4s;
  width: 13px;
}

input:checked + .slider {
  background-color: var(--primary-color);
}

input:checked + .slider:before {
  transform: translateX(13px);
}

.slider.round {
  border-radius: 17px;
}

.slider.round:before {
  border-radius: 50%;
}

.dark-mode {
  float: right;
  border: 1px solid var(--secondary-color);
  padding: var(--global-space);
  display: flex;
  align-items: center;

}

.dark-mode em {
  margin-right: 10px;
  font-size: 1rem;
}
  </style>
</head>
<body class="terminal">
<div style="margin: 2% 0 10% 15%; width: 45%;">

<div class="dark-mode">
<em>dark mode: </em>
<div class="theme-switch-wrapper">
<label class="theme-switch" for="checkbox">
<input type="checkbox" id="checkbox" />
<div class="slider round"></div>
</label>
</div>
</div>

# B·o·B, an universal & secure peer-to-peer file-transfer in OCaml

Bob is a software that allows you to transfer a file from one person to another
in a secure way. The software requires no installation, no registration and is
not a monetised service. The service cannot inspect the content of the
documents exchanged. It respects your privacy in many levels. It simply allows
the exchange of documents for all, as a fundamental mean of Internet users.

The software is available here
<img style="width: 18px;" height="20" src="https://worker.jart.workers.dev/redbean/linux.png" alt="linux" />
<img style="width: 20px;" height="20" src="https://worker.jart.workers.dev/redbean/windows10.png" alt="windows" />
<img style="width: 20px;" height="20" src="https://worker.jart.workers.dev/redbean/msdos60.png" alt="msdos" />
<img style="width: 18px;" height="20" src="https://worker.jart.workers.dev/redbean/macos.png" alt="macos" />
<img style="width: 20px;" height="20" src="https://worker.jart.workers.dev/redbean/freebsd64.png" alt="freebsd" />
<img style="width: 22px;" height="20" src="https://worker.jart.workers.dev/redbean/openbsd.png" alt="openbsd" />
<img style="width: 20px;" height="20" src="https://worker.jart.workers.dev/redbean/netbsd2.png" alt="netbsd" />:
[bob.com (x86\_64)][bob-com]

You can follow the project via [this Atom feed][feed].

<ol class="terminal-toc">
<li>[How to use it?][]</li>
<li>[The relay][]</li>
<li>[Questions and Answers][]</li>
<li>[Manifest][]</li>
</ol>

## How to use it?

### On Windows

Windows support focuses on simplified use of Bob (which can, however, be
completed if you are comfortable with the command line). Uploading a file is a
drag'n'drop of the file to the downloaded `bob.com` software:

<p align="center">
<img src="https://github.com/dinosaure/bob/blob/d313645e0cf4a76b46c26ce1b5a3aa1f807b927b/img/send.gif?raw=true" style="width: 60%;" />
</p>

The user must then share the given password with the person who wants to
receive the file. The latter can receive the file by double-clicking on
`bob.com` and inserting the given password:

<p align="center">
<img src="https://github.com/dinosaure/bob/blob/d313645e0cf4a76b46c26ce1b5a3aa1f807b927b/img/receive.gif?raw=true" style="width: 60%;" />
</p>

### On other platforms, from a shell

For all other platforms but also on Windows<sup>[1](#fn1)</sup>, Bob can be
used from a shell and it has several options. `bob.com --help` can help you
with an exhaustive description of the available options.

The basic use of Bob is to give an existing file/folder or a password as
an argument:
```bash
$ ls example.txt
example.txt
$ bob.com example.txt
Password: crulgansogglespi-gunkelfaby
$ bob.com crulgansogglespi-gunkelfaby
Accept from 213.245.183.59:52014 [Y/n]: Y
>>> Received a file: example.txt.
```

More concretely, bob.com offers 3 options:

- `bob send` to send a file
- `bob recv` to receive a file
- `bob relay` to initiate a _relay_

#### Bob for a sender

To send a file, simply run `bob.com send` with the file location. Bob will
generate a password which you will need to pass to the person you want to send
the file to.

You can decide on several parameters:

- you can decide the password with the `-p` option
- you can ask Bob not to compress the file with the `--no-compression` option,
  this can be efficient for sending videos/images
- you can specify the relay address. By default, Bob uses the relay available
  on [osau.re][osau.re] but you can deploy your own service and use the `-r`
  option. You can specify a domain name or an IP address.
- by default, Bob will resolve domain names via [uncensoreddns][uncensoreddns]
  but the user can specify their own DNS resolver with the `--nameserver`
  option.

#### DNS resolution

For this last option, the argument format can include several pieces of
information such as the fingerprint of the TLS certificate used by the DNS
resolver. Here are some examples of how to use `--nameserver`.
```bash
# https://blog.uncensoreddns.org/dns-servers/ shows public keys available for
# DNS over TLS communication with their servers. We took the ECDSA public key
# for unicast.censurfridns.dk
$ cat key.pem
-----BEGIN PUBLIC KEY-----
MHYwEAYHKoZIzj0CAQYFK4EEACIDYgAEkCMVe3bBUpgKecLOPWfe62eivwwQ1t6C
pXjH0L8sC3XrrsJYmjIuWuy+g4beaDhQyI5sM/3R2wIDswuWW4qiVUyP/PU767Bw
Td8FE0gf+mV90PFHVmGcdabPtAngFADZ
-----END PUBLIC KEY-----
$ openssl pkey -inform pem -in ecdsa.pem -pubin -pubout -outform DER | \
  openssl dgst -sha256 -binary | \
  base64
INSZEZpDoWKiavosV2/xVT8O83vk/RRwS+LTiL+IpHs=
$ bob.com send --nameserver \
  'tls:89.233.43.71!key-fp:INSZEZpDoWKiavosV2/xVT8O83vk/RRwS+LTiL+IpHs='
  example
# You can download the certificate with openssl and extract the fingerprint
# to use it then
$ openssl s_client -connect 89.233.43.71:853 </dev/null 2>/dev/null | \
  openssl x509 -fingerprint -sha256 -noout | \
  cut -d'=' -f2 | tr -d ':' | xxd -r -p | base64 -
ZGDOiBng2T0tx11GsrQDifAV8hVWFcI8kBfqz4mf9U4=
$ bob.com send --nameserver \
  'tls:89.233.43.71!cert-fp:sha256:ZGDOiBng2T0tx11GsrQDifAV8hVWFcI8kBfqz4mf9U4='
  example
```

#### Bob as a receiver

For reception, the `--nameserver` and `-r` options are the same but the
software asks directly for the password. Then, it proposes 2 main options:

- The `-o` option to define the destination of your file
- The `-y` option which automatically accepts any file received from a peer
  with the same password as you just entered.

Indeed, Bob requires, by default, a human action to accept (or not) a file. When
making this request, it gives the IP address of the sender as a means of
identification.

Other options exist such as `-v` or `--temp` but you can refer to the help
offered by `bob.com` through the `--help` option for more details.

### Issues

Bob is what is known as a polyglot program. The same program works on several
platforms. However, it happens that some platforms misinterpret `bob.com`. At
this point, there are mainly 2 errors:

1) If you are a `zsh` user, your shell will probably not recognize `bob.com` as
   an executable.
2) If you are a `binfmt_misc` user, it will try to interpret `bob.com` as a
   [Wine][wine] (or [Mono][mono]) program

The first problem has been fixed with the recent version of `zsh` (5.9.0).

The second problem requires `binfmt_misc` to correctly recognise polyglot
programs. An installation is required and available
[here][ape-loader-binfmt_misc].

More pragmatically, `bob.com` can become a truly native program to your
platform. Indeed, there is an option to turn it into an executable native to
your platform:
```bash
$ file bob.com
bob.com: DOS/MBR boot sector
$ sh -c "./bob.com --assimilate"
$ file bob.com
bob.com: ELF 64-bit LSB executable, x86_64
```

This is called assimilation of the program to your platform.

## The relay

The Bob software also offers the implementation of a relay. We need to describe
a little more precisely how bob works and the `-r` option available for
receiving and sending a document.

Indeed, Bob passes through an intermediary in sending and receiving. We call it
the relay. The latter stabilises the connection and avoids a situation of
censorship of one of the two protagonists. It also allows the identity of
senders and receivers to be arbitrated.

More pragmatically, peer-to-peer file transfer is still difficult to envisage
when most Internet service providers refuse to allow their customers to
initiate a connection. To overcome this problem, the relay exists and blindly
transmits the information of the sender and the receiver. A challenge between
all senders and receivers is required to associate peers with the same
password.

When this association is made, the communication between the two peers becomes
encrypted and only the peers can decrypt the content. The relay, again, only
satisfies the transmission by having no means of decrypting the communications.

This requires that users still trust the relay administrator. By default, the
relay available at [osau.re][osau.re] is used (administered by your humble
servant). However, we leave it up to the user to deploy their own relay and use
it instead of ours.

The relay can simply be run with:
```bash
$ ./bob.com relay
```

You can specify few options and one argument:

- `bob.com relay` can be configured to be bound on a specific IP address
- the PID of the relay can be written to a special file with the `-p` option
- you can _daemonize_ the relay with `--daemonize`
- you can specify how long a peer can stay connected without finding its
  counterpart with the `-t` option (in seconds).

Other common options exist and we invite you to discover them with
`bob.com relay --help`. The relay can be switched off properly with the SIGINT
signal (^C).

### The relay as an unikernel ([MirageOS][mirage])

Bob is a MirageOS project and, as such, offers a relay as a unikernel. You can
learn more about unikernels on the official MirageOS website or on the
[robur.coop][robur] website (which implements a series of unikernels).

It is therefore possible to deploy a relay as a fully-fledged operating system
and virtualise it with [KVM][kvm] or [Xen][xen]. There are other targets for a
MirageOS project such as [seccomp][seccomp] or virtio (for
[Google Cloud][google-cloud]) but we will concentrate on deploying a relay on
KVM here.

Like any MirageOS project, you need to install the Mirage tool via
[OPAM][opam].
```bash
$ sudo apt install opam
$ opam init -y
$ opam switch create 4.14.0
$ opam install mirage
$ eval $(opam env)
```

Using the mirage tool, it is possible to compile/produce the unikernel in this
way:
```bash
$ git clone https://github.com/dinosaure/bob
$ cd bob
$ opam pin add -y .
$ mkdir ../bob-unikernel
$ cp unikernel/* ../bob-unikernel/
$ cd ../bob-unikernel/
$ mirage configure -t hvt
$ make depends
$ mirage build
$ ls dist/bob.hvt
dist/bob.hvt
```

An image `bob.hvt` is crafted and it can be launched with [Solo5][solo5] and
[albatross][albatross]. Albatross is available _via_ `apt` if you want:
```bash
$ wget -qO- https://apt.robur.coop/gpg.pub | apt-key add -
$ echo "deb https://apt.robur.coop ubuntu-20.04 main" >> /etc/apt/sources.list
$ sudo apt update
$ sudo apt install solo5-hvt albatross
```

The construction of the relay as a unikernel for Solo5 is proposed by your
humble servant through our infrastructure which ensures its reproducibility:
[builds.osau.re/bob-hvt][bob-hvt]

#### Networks and unikernels

For OS virtualization, you usually requires a bridge:
```bash
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
```bash
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
    -p tcp -m tcp --dport 9001 =j DNAT --to-destination 10.0.0.2:9001
```

This deployment method is typical of deploying a unikernel on a machine that has
KVM. Of course, other methods exist and depend on your infrastructure. Deploying
on Google Cloud can be quite different but we advise you to refer to
[our repository][github-repository] which can contain all the necessary
information for such infrastructures.

#### Launch the unikernel

You can finally launch the unikernel with `albatross` with:
```bash
$ albatross-client-local create --net=service \
    --arg="--ipv4=10.0.0.2/24"
    --arg="--ipv4-gateway=10.0.0.1" bob bob.hvt
```

## Questions and Answers

**Q**: I would like to send several files with bob, how can I do that? \
**A**: The best way to send multiple files is to put them all in one folder and
send it with `bob.com`:
```bash
$ mkdir dir
$ cp ../file0 ../file1 ../file2 dir/
$ ./bob.com send dir/
```

**Q**: `bob.com` takes a long time to compress my file, how can I make it
faster? \
**A**: By default, `bob.com` compresses files and folders, but in the case of
some files (such as videos, images, etc.), compression is inefficient (and
slow). However, we don't want to infer the content of what you want to send (to
protect your privacy) - so it's up to you to recognise whether using
`--no-compression` is appropriate or not.
```bash
$ ./bob.com send --no-compression House.Of.The.Dragons.S01E02.mkv
```

**Q**: It would be very interesting to have a graphical interface to Bob,
would it be possible to have one (for Windows for example)? \
**A**: Unfortunately, Bob's main focus is on portability. In this respect, a
graphical interface would pose far more problems than it can solve. We have
focused all our efforts on making the software easy to use and we consider it
healthy to ask the user to understand how to use Bob.

**Q**: The relay is inaccessible to me (censorship, intranet, etc.), would it
be possible to send a file to someone anyway? \
**A**: If you are subject to censorship, there may be alternatives to the
[osau.re][osau.re] relay that can be accessed. We therefore invite you to look
for such relays (and we endeavour to maintain a list of available relays).

For an intranet, we aim to allow file transfer in such a context but this
requires a little more work. An intermediate solution would be to ask your
network administrator to deploy a relay. 

**Q**: Is it safe to use my own password to transmit a file? \
**A**: Even though your password will never be transmitted over the network, we
do not recommend that you use your password. Indeed, it is more secure to let
`bob.com` generate a password for you.

The first problem is that an attacker could infer your password through the
relay (with a dictionary attack). The second problem is that we humans have
fairly predictable password ideas - an attacker could get your file if he/she
infers your password faster than the expected receiver.

**Q**: If I want to deploy a relay, how many resources (bandwidth, power,
memory) do I need? \
**A**: The relay is very simple and only transmits the information. Only the
_"handshake"_ (when a sender looks for his/her receiver), is expensive but
remains finally rather simple (the protocol is not very complicated and the
search for agreements is possible only after the transmission of few
information).

It all depends on how you want to deploy your relay. Bob is a MirageOS project.
You can deploy the relay as an operating system, as a binary with capabilities
([seccomp][seccomp]) or as an image for [Google Cloud][google-cloud].

More concretely, the Bob unikernel is ~3 MB and only needs 512 MB of memory
(probably less) to work properly. The resources required are therefore very
minimal. For example, Google Cloud offers such an instance (`f1-micro`) for 4$
per month...

**Q**: Can I contribute to the project and suggest improvements? \
**A**: Yes, the project is a free project (under the MIT license) and you can
propose changes to improve Bob on its [GitHub repository][github-repository].

**Q**: Your project is very cool, can we help in any way the team in charge of
its development? \
**A**: As we have said, this project is not a monetised service. We therefore
use our own resources (in time and infrastructure) to maintain this project.
The best way to help us is to donate on the official website of
[robur.coop][donate] (the association in charge of Bob).

**Q**: `bob.com` tells me that it could not save the document because it
already exists. What should I do? \
**A**: `bob.com` does not want to be intrusive on your system. If the name of
the uploaded document already exists on your computer, we will preserve your
files and save the uploaded document elsewhere. Bob informs you that he will
save the document in a temporary location (for Linux for example, it will save
the location in `/tmp`). You can also specify the location where you want to
save the document with the `-o` option.

**Q**: File transfer is not as fast as other services. What should I do? \
**A**: A speed problem may exist and concerns all 3 parties involved in the
transfer: you, the relay and the receiver. The transfer speed is essentially
the one with the lowest transfer speed. To this we add that the resources
(especially the relay) are offered to you free of charge. It is therefore
appropriate that they do not correspond in their specifics to what competing
paid services offer.

**Q**: `bob.com` tells me that the file is corrupt. What should I do? \
**A**: There are many reasons why there may be corruption of the document
received. Indeed, there may have been an interruption in the file transfer.
`bob.com` is trying to protect you first and foremost. The received document
should be deleted and you should ask the sender to send you the document again.

**Q**: Why do I need to download `bob.com`? Don't you have an online service
available where no download is required? \
**A**: This problem concerns more specifically the notion of security. Creating
an online service in which people can transmit their files requires that they
trust our relay, our website (offering the service) and our code. In contrast,
as far as `bob.com` is concerned, you need to trust only our relay and our code
\- you can even deploy your own relay!

Of course, this implies a barrier in the use of Bob that requires the user to
download our software. But we consider this to be a good idea in view of what
it can bring us in terms of security.

We are however open to the development of such a service but we need to
correctly identify the points concerning security first.

**Q**: I am a company and would like to use Bob internally. How do I do this
and what do I need? \
**A**: Bob can be deployed internally and your users will just need to specify
the correct address with the `-r` option to communicate with your relay. You
have several deployment methods:

- Using `bob.com relay` in the background on your server. This option is the
  least difficult to set up. It only requires you to run `bob.com` on a server
  which your users can access.
- Deploy a unikernel with MirageOS. This option is the most difficult as it
  involves creating a unikernel with MirageOS and deploying it as an operating
  system on a server. The latter must have virtualisation with KVM or Xen. We
  advise you to follow the tutorial on our official repository
  [here][github-repository].

The second method is however more interesting as the relay will not have the
means to directly interact with (and perhaps infect) the host system. A
fundamental barrier exists between the unikernel and the host system. As far as
[osau.re][osau.re] is concerned, we have deployed the relay with [Solo5][solo5]
on KVM.

**Q**: Can I trust the downloaded binary? \
**A**: Our association has made a definite effort to offer a platform that
ensures the "reproducibility" of the software production (between the sources
available here and the binary you download). This proof ensures that if you
follow the steps to build Bob from a "context" described
[here][reproducible-bob], you will get exactly the same thing.

This method allows us to build a relationship of trust between you and us.

**Q**: If I find a bug, what is the best way to help?
**A**: Bob has some options for giving information that can really help
developers. The first is the `-vvv` option which displays Bob's _debug_
information. The second is `--seed` (which expects a value in Base64 form) which
allows the same state to be reproduced for all generated values. The third is
`--reproduce` which admits a predictable reproduction of the shared secret key.

These options **are not** to be used in a real exchange. The predictability of
Bob's operation can be an important attack beam, it is only for debugging.

Finally, the `--reproduce` option must be used on both sides (of the sender and
receiver). The seed used must also be the same on both sides.

**Q**: What is the best way to follow the project?
**A**: We offer an [Atom feed][feed] on our website that keeps you informed of
all the progress and changes on our relay. For a more detailed tracking, we
advise you to follow our [GitHub repository][github-repository].

## Manifest

After this _brief_ introduction, we can really present `bob.com`, its
objectives and how it was designed.

The transfer of files remains an elementary action in the exchange of
information that the Internet allows. It should not be subject to any
limitations, monitoring or restrictions. However, tools and services offering
such exchange often require registration, installation, limitation or, worse,
your credit card.

Bob's goal is **simple**. To allow as many people as possible to exchange files
in a quasi-autonomous way. In this respect, the development of `bob.com` has 3
objectives:

1) Accessibility and Portability
2) Security
3) Ease of use

### Accessibility & Portability

The primary objective of `bob.com` is portability and accessibility on multiple
platforms. The world's computer population does not only run on Windows, Mac or
Linux, but there are a whole range of platforms for which there are users
(FreeBSD, OpenBSD or NetBSD).

Our primary goal is to provide **one and only** binary that can be used on all
these platforms. Behind this objective, the difficulty can be great. Indeed,
this variation of systems is concrete and has heavy implications in the
development of a software (whatever it is). 

One method to overcome this portability problem is to create a derivation of
the software for all these platforms. This means that there is a version of the
software for one platform (5 platforms imply 5 versions of the software).
Unfortunately, these derivations can lead to hard-to-find bugs as they are
often platform-specific (and not necessarily related to the software itself).
This derivation also requires quite a lot of work from the developers. It makes
us produce the same software on several platforms with their tools (different
from each other), their constraints and specificities. We are clearly not
experts on all these platforms.

However, the differences between the platforms can be a detail when it comes to
making a "simple" software like ours. For this reason, we have opted to use
[Cosmopolitan][cosmopolitan]. Cosmopolitan offers a foundation which, according
to the author, works on all these platforms. The main advantage of Cosmopolitan
is that it offers a "polyglot" binary that can run without installation and
without prerequisites. The second advantage is that the derivation described
above is no longer necessary (at least, it is less required). In this way, the
same code works on all these platforms, which makes development easier (and
avoids the introduction of new bugs). It is also enough for us to have, at
least, only one way to produce our software (only one _toolchain_) which is a
considerable saving in cost (since we do not need several machines/_toolchain_
to offer you our software).

This portability also requires us, perhaps counter-intuitively, to respect
strong constraints on what is available on all these platforms. It may be
tempting to use the advantage of certain platforms (a nice interface on
Windows, a good transmission speed on Linux, etc.) but they are contrary to the
desired "universalism" of our software.

Indeed, behind this notion of portability, there is above all the notion of
accessibility. We are not all equal in terms of the resources available to us
(whether in terms of knowledge or financial resources). We really want to take
the opposite approach from the too systematic one which consists in offering
good services only to a certain category of the population. It is therefore in
our interest to offer the possibility of exchange to everyone (even if this
implies concessions on what we ultimately consider to be the margin in view of
this first objective of accessibility).

In this sense, `bob.com`'s development is very much focused on the notion of
portability, which takes precedence over aspects of speed or user experience.
This has a real implication on our development where the question persists: is
what I am developing portable to all these platforms?

### Security

Bob would like to propose an approach where the issue of safety should be
systematic. That is to say that all improvements should be thought of through
the prism of safety at all levels.

If we were to give an example to understand this way of developing, it would be
the addition to `bob.com` of its ability to resolve a domain name. It is
something very simple that consists of letting `bob.com` resolve a domain name
like [osau.re][osau.re], know its IP address and then connect to it. This
resolution requires an external resource (a DNS resolver) which can be
legitimately questioned for its trust.

In this, we propose:

1) the possibility for the user to choose his/her DNS resolver
2) to propose a DNS resolver in line with our values
3) communicate with this DNS resolver in the most secure way possible by default

This example shows that for a simple question, several aspects are involved:

1) The ability for the user to always have a choice
2) Proposing default behaviours that do not put the user at risk
3) Always consider the highest level of security in our technical choices

In concrete terms, `bob.com` ensures that your privacy is not violated by our
intermediary, even if only through the protocol used to transmit your
documents. Only you know the password used and it is never exchanged by
`bob.com` in any way. This means that you are **responsible** for passing on
this password to the recipient.

More globally, we consider that security cannot result from an obfuscation of
the means put in place for the transfer but from making users aware of the
possible risks of corruption and violation of his/her private life. Of course,
`bob.com` tries to offer a turnkey solution in this transfer (in line with our
third point) but the notion of "secrecy" should be yours alone - and in this,
`bob.com` would not be responsible in any way.

More generally, the notion of security always requires skills, is associated
with a particular language, can always be challenged (by means or new
techniques). There is no such thing as absolute security and the fundamental
principle of trust in this notion should be the question you should ask
yourself: **do you trust us?**

To this question, and according to our vision, we consider that it should be
possible for the user to trust us and use our software in the simplest way
possible (and thus not require security skills in order to use our software) as
well as it should be possible to question our technical choices and to be able
to easily modify them in order to gradually implicate your responsibility in
this matter of security (which, in all, should only concern you).

In this, we advocate composability and configuration. Indeed, the level of
security required and/or expected is fundamentally dependent on a context that
is up to you to define. Depending on this context, Bob's default behaviour may
be sufficient for you. However, you may want to: decide on the DNS resolver
used, the relay used, the password used, etc. In this, we place a real emphasis
on the ability to configure Bob correctly and according to your needs.

However, Bob cannot offer you all the aspects required to feel protected. We
cannot integrate all existing solutions as this means either using more external
resources that we do not have control over or reimplementing existing solutions.
This is where Bob's composability comes in. Since the software is kept very
simple, we maintain different behaviours that make it composable with other
solutions like [Tor][tor] (to achieve, for example, anonymisation).

### Simplicity

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

This vision allows us to introduce the user, if he/she wishes, to aspects
related to security. Simplicity, once again, is not so much about being able to
use the software without any prerequisites (even if we focus on this issue in
terms of accessibility) but about the fact that the software can provide keys to 
understanding the security aspects.

### Conclusion

These objectives should certainly not be seen as independent but as
interdependent. Security can make the software more complex to use and can make
it less accessible as well. Simplicity can put the user's privacy at risk by
omitting details that may be important in file transmission. The sense of
simplicity can also limit the accessibility of the software by considering a
simplicity by use (building social relics that not everyone has). Finally,
accessibility is a real challenge as soon as we integrate cryptographic
techniques (are they available on all platforms, etc.) and it implies certain
contortions regarding simplicity - what is accessible is not necessarily
aligned with simplicity of use.

Bob tries to crystallize all these aspects and we know that on some, and
according to everyone's vision, the software is not perfect. Nevertheless, we
want to draw the limits of our framework in order not to catch anyone unaware
of the evolution of this software.

So this manifesto is here to lay out (as best we can) what Bob is about and
what will happen to it. It is necessary to explain all this because we know
that the trust you can place in us does not depend (fortunately) only on the
usefulness you can find in this software but also on its stability and the
team's view behind the project.

<p align="right">The [Robur][robur] Team</p>

[linux-img]: https://worker.jart.workers.dev/redbean/linux.png
[windows-img]: https://worker.jart.workers.dev/redbean/windows10.png
[dos-img]: https://worker.jart.workers.dev/redbean/msdos60.png
[macos-img]: https://worker.jart.workers.dev/redbean/macos.png
[freebsd-img]: https://worker.jart.workers.dev/redbean/freebsd64.png
[openbsd-img]: https://worker.jart.workers.dev/redbean/openbsd.png
[netbsd-img]: https://worker.jart.workers.dev/redbean/netbsd2.png
[ape-loader-binfmt_misc]: https://justine.lol/apeloader/#binfmt_misc
[bob-com]: https://builds.osau.re/job/bob/build/latest/f/bin/bob.com
[bob-hvt]: https://builds.osau.re/job/bob-hvt/build/latest/f/bin/bob.hvt
[wine]: https://www.winehq.org/
[mirage]: https://mirage.io/
[kvm]: https://www.linux-kvm.org/page/Main_Page
[xen]: https://xenproject.org/
[google-cloud]: https://cloud.google.com/
[seccomp]: https://code.google.com/archive/p/seccompsandbox/wikis/overview.wiki
[opam]: https://opam.ocaml.org/
[albatross]: https://github.com/roburio/albatross
[solo5]: https://github.com/solo5/solo5
[github-repository]: https://github.com/dinosaure/bob
[donate]: https://robur.io/Donate
[osau.re]: https://blog.osau.re/
[uncensoreddns]: https://blog.uncensoreddns.org/
[robur]: https://robur.coop/
[cosmopolitan]: https://justine.lol/cosmopolitan/
[tor]: https://www.torproject.org/
[reproducible-bob]: https://builds.osau.re/job/bob/build/latest
[mono]: https://www.mono-project.com/
[feed]: https://bob.osau.re/feed.xml

<script>
hljs.highlightAll();

const toggleSwitch = document.querySelector('.theme-switch input[type="checkbox"]');

function switchTheme(e) {
  if (e.target.checked) {
    document.documentElement.setAttribute('theme', 'dark');
    localStorage.setItem('theme', 'dark');
  } else {
    document.documentElement.setAttribute('theme', 'light');
    localStorage.setItem('theme', 'light');
  }
}

toggleSwitch.addEventListener('change', switchTheme, false);

const currentTheme = localStorage.getItem('theme') ? localStorage.getItem('theme') : null;

if (currentTheme) {
  document.documentElement.setAttribute('theme', currentTheme);

  if (currentTheme === 'dark')
    toggleSwitch.checked = true;
}
</script>
</div>
</body>
</html>
