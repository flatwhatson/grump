# GRUMP - Guile Random Utilities and Miscellaneous Programs

This is my scratch-pad for exploratory programming in Guile Scheme.  It
exists to collect functionality I've found useful while live-coding, and
capture my abandoned experiments for posterity.

## Libraries

### (grump units si)

A port of [clj-units](https://code.google.com/archive/p/clj-units/) to
Guile Scheme, using `(oop goops)` to provided overloaded mathematical
operations which preserve unit dimensions and quantities.

### (grump parse bencode)

A parser for the [Bencode](https://en.wikipedia.org/wiki/Bencode) format
using `(ice-9 peg)`.

### (grump parse tcpflow)

A parser for the output of [tcpflow](https://github.com/simsong/tcpflow)
using `(ice-9 peg)`.
