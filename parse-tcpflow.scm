#!/usr/bin/env -S guile -e main -s
!#
(use-modules (ice-9 peg)
             (ice-9 pretty-print)
             (ice-9 textual-ports))

(define parse-tcpflow
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

(define (print-help argv)
  (display (string-append "\
usage: " (car argv) " [options] [input] [output]

options:
  -h, --help       Display this help

Parse the output of \"tcpflow -D\" from input (default stdin) and write the
parsed s-exp to output (default stdout).
")))

(define (main argv)
  (if (or (member "-h" argv)
          (member "--help" argv))
      (print-help argv)
      (let* ((argc (length argv))
             (input (or (and (> argc 1) (list-ref argv 1)) "-"))
             (output (or (and (> argc 2) (list-ref argv 2)) "-"))
             (input-text (get-string-all
                          (if (equal? input "-")
                              (current-input-port)
                              (open-input-file input))))
             (output-port (if (equal? output "-")
                              (current-output-port)
                              (open-output-file output))))
        (pretty-print
         (parse-tcpflow input-text)
         output-port))))
