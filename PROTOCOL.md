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
