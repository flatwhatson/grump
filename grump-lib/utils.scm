(define-module (grump-lib utils)
  #:use-module (ice-9 format)
  #:export-syntax (print! warn! debug! time!))

(define-public %grump-debug-enabled (make-fluid #f))
(define-public %grump-debug-to-error (make-fluid #f))

(define-syntax-rule (print-port! port msg ...)
  (begin (format port msg ...)
         (newline port)))

(define-syntax-rule (print! msg ...)
  (print-port! (current-output-port) msg ...))

(define-syntax-rule (warn! msg ...)
  (print-port! (current-error-port) msg ...))

(define-syntax-rule (debug? then else ...)
  (if (fluid-ref %grump-debug-enabled)
      then
      else ...))

(define-syntax-rule (debug!! msg ...)
  (print-port!
   (if (fluid-ref %grump-debug-to-error)
       (current-error-port)
       (current-output-port)) msg ...))

(define-syntax-rule (debug! msg ...)
  (debug?
   (debug!! msg ...)))

(define-syntax-rule (time! msg action action* ...)
  (debug?
   (let* ((start (get-internal-real-time))
          (result (begin action action* ...))
          (finish (get-internal-real-time))
          (duration (/ (- finish start)
                       internal-time-units-per-second)))
     (debug!! (string-append msg " (~,3fs)") duration)
     result)
   (begin action action* ...)))
