(define-module (grump bencode)
  #:use-module (grump utils)
  #:use-module (ice-9 peg)
  #:use-module (srfi srfi-2))

(define-peg-pattern b-integer-raw body
  (and (ignore "i") (+ (range #\0 #\9)) (ignore "e")))

(define-peg-pattern b-string-length-raw body
  (and (+ (range #\0 #\9)) (ignore ":")))

(define-peg-pattern b-list all
  (and (ignore "l") (* b-value) (ignore "e")))

(define-peg-pattern b-dict all
  (and (ignore "d") (* (and b-string b-value)) (ignore "e")))

(define-peg-pattern b-value body
  (or b-integer b-string b-list b-dict))

(define (match->number val)
  (string->number
   (if (list? val)
       (apply string-append val)
       val)))

(define (b-integer str len pos)
  (and-let* ((match (b-integer-raw str len pos)))
    (let ((pos (car match))
          (num (match->number (cadr match))))
      (list pos `(b-integer ,num)))))

(define (b-string str len pos)
  (and-let* ((match (b-string-length-raw str len pos)))
    (let* ((pos (car match))
           (num (match->number (cadr match)))
           (end (+ pos num)))
      (if (<= end len)
          (list end `(b-string ,(substring str pos end)))
          (begin
            (warn! "ERROR: can't read ~a bytes from position ~a in string of length ~a"
                   num pos len)
            #f)))))

(define-public (parse-bencode input)
  (peg:tree (match-pattern b-value input)))
