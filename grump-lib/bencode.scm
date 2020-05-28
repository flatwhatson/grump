(define-module (grump-lib bencode)
  #:use-module (ice-9 peg)
  #:use-module (srfi srfi-2))

(define-public parse-bencode
  (let ()
    (define-peg-string-patterns
      "ignore-colon < ':'
       ignore-i     < 'i'
       ignore-l     < 'l'
       ignore-d     < 'd'
       ignore-e     < 'e'

       b-length-raw  <- [0-9]+ ignore-colon
       b-integer-raw <- ignore-i [0-9]+ ignore-e

       b-list  <-- ignore-l b-value* ignore-e
       b-dict  <-- ignore-d (b-string b-value)* ignore-e
       b-value <-  b-integer / b-list / b-dict / b-string ")

    (define (flatten-string elems)
      (if (list? elems)
          (apply string-append (map flatten-string elems))
          elems))

    (define (flatten-number elems)
      (string->number (flatten-string elems)))

    (define (b-integer str len pos)
      (and-let* ((match (b-integer-raw str len pos)))
        (list (car match)
              (list 'b-integer (flatten-number (cdr match))))))

    (define (b-length str len pos)
      (and-let* ((match (b-length-raw str len pos)))
        (list (car match)
              (flatten-number (cdr match)))))

    (define (b-string str len pos)
      (and-let* ((match (b-length str len pos)))
        (let* ((num (cadr match))
               (pos (car match))
               (end (+ pos num)))
          (if (<= end len)
              (list end (list 'b-string (substring str pos end)))
              (begin
                (display "ERROR: tried to read ")
                (display num)
                (display " bytes from position ")
                (display pos)
                (display " ending at ")
                (display end)
                (display " in string of length ")
                (display len)
                (newline)
                #f)))))

    (define (wrap name proc)
      (lambda (str len pos)
        (display name)
        (display "...")
        (newline)
        (let ((result (proc str len pos)))
          (display name)
          (display ": ")
          (write result)
          (newline)
          result)))

    (set! b-length  (wrap "b-length"  b-length))
    (set! b-string  (wrap "b-string"  b-string))
    (set! b-integer (wrap "b-integer" b-integer))
    (set! b-list    (wrap "b-list"    b-list))
    (set! b-dict    (wrap "b-dict"    b-dict))

    (lambda (input)
      (peg:tree (match-pattern b-value input)))))
