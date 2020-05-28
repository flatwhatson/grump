#!/usr/bin/env -S guile -e main -L . -s
!#
(use-modules (ice-9 pretty-print)
             (ice-9 textual-ports)
             (grump-lib bencode))

(define (print-help argv)
  (display (string-append "\
usage: " (car argv) " [options] [input] [output]

options:
  -h, --help       Display this help

Parse bencoded data from input (default stdin) and write the parsed
s-exp to output (default stdout).
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
         (parse-bencode input-text)
         output-port))))
