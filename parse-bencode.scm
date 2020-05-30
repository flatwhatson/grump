#!/usr/bin/env -S guile -e main -L . -s
!#
(use-modules (ice-9 binary-ports)
             (ice-9 getopt-long)
             (ice-9 iconv)
             (ice-9 pretty-print)
             (grump-lib bencode)
             (grump-lib utils))

(define (print-help args)
  (display (string-append "\
usage: " (car args) " [options] [input] [output]

options:
  -h, --help       Display this help
  -d, --debug      Print debug logs to stderr

Parse bencoded data from input (default stdin) and write the parsed
s-exp to output (default stdout).
")))

(define (main args)
  (let* ((option-spec '((help (single-char #\h) (value #f))
                        (debug (single-char #\d) (value #f))))
         (options         (getopt-long args option-spec))
         (help-wanted     (option-ref options 'help #f))
         (debug-wanted    (option-ref options 'debug #f))
         (extra-args      (option-ref options '() '()))
         (extra-arg-count (length extra-args))
         (input-file      (if (> extra-arg-count 0) (car extra-args) "-"))
         (output-file     (if (> extra-arg-count 1) (cadr extra-args) "-")))
    (if help-wanted
        (print-help args)
        (begin
          (when debug-wanted
            (fluid-set! %grump-debug-enabled #t)
            (fluid-set! %grump-debug-to-error #t))
          (let* ((input-port (if (equal? input-file "-")
                                 (current-input-port)
                                 (open-input-file input-file)))
                 (output-port (if (equal? output-file "-")
                                  (current-output-port)
                                  (open-output-file output-file)))
                 (input-bytes
                  (time! "Read input bytes"
                         (get-bytevector-all input-port)))
                 (input-text
                  (time! "Convert to string"
                         (bytevector->string input-bytes "ISO-8859-1")))
                 (parsed-data
                  (time! "Parse bencoded data"
                         (parse-bencode input-text))))
            (time! "Pretty-print parsed data"
                   (pretty-print parsed-data output-port)))))))
