(define-module (grump-lib bencode)
  #:use-module (grump-lib utils)
  #:use-module (ice-9 peg)
  #:use-module (srfi srfi-2))

(define-peg-string-patterns
  "ignore-colon < ':'
   ignore-i     < 'i'
   ignore-l     < 'l'
   ignore-d     < 'd'
   ignore-e     < 'e'

   b-integer-raw       <- ignore-i [0-9]+ ignore-e
   b-string-length-raw <- [0-9]+ ignore-colon

   b-list <-- ignore-l b-value* ignore-e
   b-dict <-- ignore-d (b-string b-value)* ignore-e

   b-value <- b-integer / b-string / b-list / b-dict ")

(define (match->number val)
  (string->number
   (if (list? (car val))
       (apply string-append (car val))
       (car val))))

(define (b-integer str len pos)
  (and-let* ((match (b-integer-raw str len pos)))
    (let ((pos (car match))
          (num (match->number (cdr match))))
      (list pos `(b-integer ,num)))))

(define (b-string str len pos)
  (and-let* ((match (b-string-length-raw str len pos)))
    (let* ((pos (car match))
           (num (match->number (cdr match)))
           (end (+ pos num)))
      (if (<= end len)
          (list end `(b-string ,(substring str pos end)))
          (begin
            (warn! "ERROR: can't read ~a bytes from position ~a in string of length ~a"
                   num pos len)
            #f)))))

(define-public (parse-bencode input)
  (peg:tree (match-pattern b-value input)))
