# Protocol

The protocol has a very simple format. It consists of a number in hexadecimal
representation corresponding to a unique identifier that only the relay can
assign. This unique identifier is between 1 and 65535 (0xFFFF). The identifier
0 is reserved for the relay.

The identifier corresponds to the source if it is a received packet or to the
destination if it is a packet to be sent.

Then, a number in hexadecimal value follows corresponding to the type of the
packet. There are 16 types of packets.

```
+-+-+-+-+      +-+-+
| | | | |      | | |
+-+-+-+-+      +-+-+

   UID     packet's type
```

Finally, depending on the packet, information may follow with a particular
delimiter. The information can be of a fixed size (for packet `00`, 34 bytes
are expected next) or end with a particular character (for packet `02`, the
information ends with `'\000'`).

## The handshake

The handshake takes place in several steps between the peers. The first packet
sent is a "Hello" which identifies the peer as a "client" or "server". The
difference between client and server is the following: the server wants to send
a file, the client wants to receive a file. It is with this "Hello" packet that
the relay assigns a unique number to these peers.

**NOTE**: it is **impossible** for a client to want to send a packet to
another peer identified as a client. The same is true for a server.

The `Hello_as_a_server` also gives information such as the salt and the KDF
algorithm used. The format of this information is that proposed by
[spoke][spoke] - it is the `public` element that the server must generate. The
size of this information is always 34 bytes. These packets are always destined
to the relay.

````
| destination | packet |    data    |
    [ 0000 ]    [ 00 ]   [ public ]   => `Hello_as_a_server public
    [ 0000 ]    [ 01 ]                => `Hello_as_a_client
```

When the relay receives these packets, it can assign a unique identifier to
these peers. It then responds with their "identities" to the relay. This
identity corresponds to the serialization of the `sockaddr` - its size should
not exceed 53 bytes (47 bytes for the IP address and 6 bytes for the port).
The information is delimited by an `'\000'`. Only the relay is able to send
this kind of packet.

```
|  source  | packet |        data         |
  [ 0000 ] | [ 02 ] | <addr>:<port>'\000' | => `Server_identity <addr>:<port>
  [ 0000 ] | [ 03 ] | <addr>:<port>'\000' | => `Client_identity <addr>:<port>
```

### Broadcast

At this stage, peers are waiting to meet new peers. The first to react is the
client to which we will send all available servers according to the relay. In
other words, when a client connects, the relay sends to it all available
servers. When a server connects, the relay sends to all available clients the
accessibility of this new server. This is called "broadcasting".

Only the relay is able to send this kind of packet and only a client is willing
to handle this kind of packet.

```
|  source  | packet |                    data                     |
  [ 0000 ]   [ 04 ]   [ xxxx ] [ public ] [ <addr>:<port>'\000' ]

  => `New_server (uid, public, identity)
```

[spoke]: https://github.com/dinosaure/spoke
