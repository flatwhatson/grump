(define-module (grump-lib utils)
  #:export-syntax (print! debug! warn!))

(define-public %grump-debug-enabled (make-fluid #f))
(define-public %grump-debug-to-error (make-fluid #f))

(define-syntax-rule (debug! msg ...)
  (when (fluid-ref %grump-debug-enabled)
    (if (fluid-ref %grump-debug-to-error)
        (print! msg ...)
        (warn! msg ...))))

(define-syntax-rule (print! msg ...)
  (format #t msg ...))

(define-syntax-rule (warn! msg ...)
  (format (current-error-port) msg ...))
