# tidal-websocket

A websocket server for Tidal. Currently, you send it Tidal code, and
it parses and runs it. Should be secure, but no promises! Released
under the terms of GNU Public License 3.0.

This is a working prototype, the protocol is likely to change.

Inspired by work of @d0kt0r0 et al on
  https://github.com/d0kt0r0/extramuros

# Protocol

The sever currently listens on port 9162.

On connection the client should receive the message `/welcome n` where
`n` is an integer unique to the current server process.

The client will then periodically receive the message `/bang n`, where
`n` is the value of the current tick when the message was
composed. This will not take account of network transmission time.

The client can send a message `/eval x` where `x` is a Tidal
pattern. This may include carriage returns, for example:

```
/eval sound "bd sn"
  # vowel "{a e i o u}%1"
```

If the pattern evaluates successfully, the client will receive the
message `/eval t x`, where `t` is the time (seconds since epoch in
decimal) of evaluation and `x` is the pattern that was evaluated.

If the pattern does not evaluate successfully, the client will receive
the message `/error t x`, where `t` is the time of failure and `x` is
the error message from the compiler. (note that due to current
internal limitations, there may be a slight delay before further
`/eval` messages will be processed, in the meantime they will be
queued).

There are experimental commands concerning tempo:

* `/faster` speeds up the tempo, currently by `0.01` cycles per second.
* `/slower` slows down the tempo, currently by `0.01` cycles per second.
* `/nudge t` bumps the clock forward (or if negative number is given,
  backwards) by `t` seconds, in effect changing the phase of the
  cycle.

Please share reports of success / failure with Alex (yaxu) on the
TOPLAP slack (https://toplap.org/toplap-on-slack/) or via email:
alex@slab.org

Patches welcome.
