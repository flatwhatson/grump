(define-module (grump tcpflow)
  #:use-module (grump utils)
  #:use-module (ice-9 peg))

(define-peg-string-patterns
  "space   < ' '
   newline < '\n'
   dot     < '.'
   dash    < '-'
   colon   < ':'
   zero    < '0'
   any     < !'\n' .

   ipv4-octet <- [1-9] [0-9] [0-9] /
                 zero ([1-9] [0-9] /
                       zero  [1-9])

   tcp-port <-- [1-9] [0-9] [0-9] [0-9] [0-9] /
                zero ([1-9] [0-9] [0-9] [0-9] /
                      zero ([1-9] [0-9] [0-9] /
                            zero ([1-9] [0-9] /
                                  zero  [1-9])))

   ipv4-address  <-- ipv4-octet '.' ipv4-octet '.' ipv4-octet '.' ipv4-octet
   host-and-port <-  ipv4-address dot tcp-port

   nibble <- [0-9] / [a-f] / [A-F]
   byte   <- nibble nibble
   offset <  byte byte colon space
   bytes  <- (byte byte space)* (byte space)?

   source      <-- host-and-port dash
   destination <-- host-and-port colon space* newline
   data        <-- (offset bytes any* newline)*

   flow        <-- source destination data newline
   flows       <-  flow* !. ")

(define-public (parse-tcpflow input)
  (peg:tree (match-pattern flows input)))
