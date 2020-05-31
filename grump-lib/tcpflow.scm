(define-module (grump-lib tcpflow)
  #:use-module (grump-lib utils)
  #:use-module (ice-9 peg))

(define-public parse-tcpflow
  (let ()
    (define-peg-string-patterns
      "space   < ' '
       newline < '\n'
       dot     < '.'
       dash    < '-'
       colon   < ':'
       zero    < '0'
       any     < !'\n' .

       ipv4-octet <- zero  zero  [1-9] /
                     zero  [1-9] [0-9] /
                     [1-9] [0-9] [0-9]

       tcp-port <-- zero  zero  zero  zero  [1-9] /
                    zero  zero  zero  [1-9] [0-9] /
                    zero  zero  [1-9] [0-9] [0-9] /
                    zero  [1-9] [0-9] [0-9] [0-9] /
                    [1-9] [0-9] [0-9] [0-9] [0-9]

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

    (lambda (input-string)
      (peg:tree (match-pattern flows input-string)))))
