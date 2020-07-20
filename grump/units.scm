(define-module (grump units)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (compatible?
            convert-to)
  #:export-syntax (define-unit-system
                   define-dimension
                   define-dimensions
                   define-unit
                   define-units
                   define-prefixed-units))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <unit-system> ()
  (name              #:init-keyword #:name              #:getter name)
  (base-dimensions   #:init-keyword #:base-dimensions   #:getter base-dimensions)
  (base-unit-names   #:init-keyword #:base-unit-names   #:getter base-unit-names)
  (base-unit-symbols #:init-keyword #:base-unit-symbols #:getter base-unit-symbols)
  (dimensions        #:init-form (make-hash-table)      #:getter dimensions)
  (units             #:init-form (make-hash-table)      #:getter units))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <dimension> ()
  (unit-system #:init-keyword #:unit-system #:getter unit-system)
  (exponents   #:init-keyword #:exponents   #:getter exponents)
  (name        #:init-keyword #:name        #:getter name))

(define-method (equal? (a <dimension>) (b <dimension>))
  (and (eq?    (unit-system a) (unit-system b))
       (equal? (exponents a)   (exponents b))
       (equal? (name a)        (name b))))

(define-method (compatible? (d1 <dimension>) (d2 <dimension>))
  (or (eq? d1 d2)
      (and (eq? (unit-system d1)
                (unit-system d2))
           (equal? (exponents d1)
                   (exponents d2))
           (or (not (name d1))
               (not (name d2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <a-quantity> ())

(define-method (equal? (a <a-quantity>) (b <a-quantity>))
  (and (equal? (dimension a) (dimension b))
       (= (magnitude-in-base-units a)
          (magnitude-in-base-units b))))

(define-method (compatible? (a <a-quantity>) (b <a-quantity>))
  (compatible? (dimension a) (dimension b)))

(define-method (compatible? (a <a-quantity>) (d <dimension>))
  (compatible? (dimension a) d))

(define-method (compatible? (d <dimension>) (b <a-quantity>))
  (compatible? d (dimension b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <unit> (<a-quantity>)
  (factor    #:init-keyword #:factor    #:getter factor)
  (dimension #:init-keyword #:dimension #:getter dimension)
  (name      #:init-keyword #:name      #:getter name)
  (symbol    #:init-keyword #:symbol    #:getter symbol))

(define-method (magnitude (this <unit>))
  1)

(define-method (magnitude-in-base-units (this <unit>))
  (factor this))

(define-method (unit (this <unit>))
  this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <quantity> (<a-quantity>)
  (magnitude #:init-keyword #:amount #:getter magnitude)
  (unit      #:init-keyword #:unit   #:getter unit))

(define-method (dimension (this <quantity>))
  (dimension (unit this)))

(define-method (magnitude-in-base-units (this <quantity>))
  (* (magnitude this)
     (factor (unit this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-same-unit-system d1 d2)
  (unless (eq? (unit-system d1) (unit-system d2))
    (error (format #f "Can't combine ~a and ~a" d1 d2))))

(define (assert-same-unit-systems d . dims)
  (for-each (cut assert-same-unit-system d <>) dims))

(define (assert-compatible-dimension d1 d2)
  (unless (compatible? d1 d2)
    (error (format #f "Can't convert ~a to ~a" d1 d2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-quantity amount unit)
  (make <quantity> #:amount amount #:unit unit))

(define* (make-dimension unit-system exponents #:optional dim-name)
  (let* ((dimension (make <dimension>
                      #:unit-system unit-system
                      #:exponents   exponents
                      #:name        dim-name))
         (dimensions (dimensions unit-system))
         (existing   (hash-ref dimensions exponents)))
    (cond ((not existing)
           (hash-set! dimensions exponents dimension))
          ((name existing)
           (hash-set! dimensions exponents
                      (make <dimension>
                        #:unit-system unit-system
                        #:exponents   exponents
                        #:name        #f))))
    (when dim-name
      (hash-set! dimensions dim-name dimension))
    dimension))

(define (get-dimension unit-system exponents)
  (let* ((dimensions (dimensions unit-system))
         (existing   (hash-ref dimensions exponents)))
    (or existing
        (let ((dimension (make <dimension>
                           #:unit-system unit-system
                           #:exponents   exponents
                           #:name        #f)))
          (hash-set! dimensions exponents dimension)
          dimension))))

(define* (make-unit factor dimension #:optional name symbol)
  (let* ((unit (make <unit>
                 #:factor    factor
                 #:dimension dimension
                 #:name      name
                 #:symbol    symbol))
         (unit-system (unit-system dimension))
         (units       (units unit-system)))
    (hash-set! units (list dimension factor) unit)
    (when name
      (hash-set! units name unit))
    unit))

(define (get-unit factor dimension)
  (let* ((unit-system (unit-system dimension))
         (units       (units unit-system))
         (existing    (hash-ref units (list dimension factor))))
    (or existing
        (let ((unit (make <unit>
                      #:factor    factor
                      #:dimension dimension
                      #:name      #f
                      #:symbol    #f)))
          (hash-set! units (list dimension factor) unit)
          unit))))

(define (as-unit quantity name symbol)
  (let ((d (dimension quantity))
        (f (magnitude-in-base-units quantity)))
    (make-unit f d name symbol)))

(define (invert-unit x)
  (let* ((u (unit x))
         (d (dimension u)))
    (get-unit
     (/ 1 (factor u))
     (get-dimension
      (unit-system d)
      (map - (exponents d))))))

(define (convert-to new-unit quantity)
  (let ((old-unit (unit quantity)))
    (if (eq? new-unit old-unit)
        quantity
        (begin
          (assert-compatible-dimension (dimension old-unit)
                                       (dimension new-unit))
          (make-quantity (* (magnitude quantity)
                            (/ (factor old-unit)
                               (factor new-unit))) new-unit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method (* (d1 <dimension>) (d2 <dimension>))
  (assert-same-unit-system d1 d2)
  (get-dimension
   (unit-system d1)
   (map + (exponents d1) (exponents d2))))

(define-method (/ (d1 <dimension>) (d2 <dimension>))
  (assert-same-unit-system d1 d2)
  (get-dimension
   (unit-system d1)
   (map - (exponents d1) (exponents d2))))

(define-method (expt (d <dimension>) n)
  (get-dimension
   (unit-system d)
   (map (cut * n <>) (exponents d))))

(define-method (sqrt (d <dimension>))
  (expt d 1/2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-unary-operator* op)
  (define-method (op (x <a-quantity>))
    (make-quantity (op (magnitude x)) (unit x))))

(define-syntax-rule (define-commutative-operator* op)
  (begin
    (define-method (op (x <a-quantity>) y)
      (make-quantity (op (magnitude x) y) (unit x)))
    (define-method (op x (y <a-quantity>))
      (make-quantity (op x (magnitude y)) (unit y)))))

(define-syntax-rule (define-commutative-predicate* op)
  (begin
    (define-method (op (x <a-quantity>) y)
      (op (magnitude x) y))
    (define-method (op x (y <a-quantity>))
      (op x (magnitude y)))))

(define-syntax-rule (define-divide-operator* op)
  (begin
    (define-method (op (x <a-quantity>) (y <a-quantity>))
      (let* ((ux (unit x))
             (uy (unit y))
             (d (/ (dimension ux) (dimension uy)))
             (f (/ (factor    ux) (factor    uy)))
             (v (op (magnitude x) (magnitude y))))
        (if (every zero? (exponents d))
            (* f v)
            (make-quantity v (get-unit f d)))))

    (define-method (op (x <a-quantity>) y)
      (make-quantity (op (magnitude x) y) (unit x)))

    (define-method (op x (y <a-quantity>))
      (make-quantity (op x (magnitude y)) (invert-unit y)))))

(define-syntax-rule (define-divmod-operator* op)
  (begin
    (define-method (op (x <a-quantity>) (y <a-quantity>))
      (let*-values (((ux) (unit x))
                    ((uy) (unit y))
                    ((d) (/ (dimension ux) (dimension uy)))
                    ((f) (/ (factor    ux) (factor    uy)))
                    ((q r) (op (magnitude x) (magnitude y))))
        (if (every zero? (exponents d))
            (values (* f q)
                    (* f r))
            (let ((u (get-unit f d)))
              (values (make-quantity q u)
                      (make-quantity r u))))))

    (define-method (op (x <a-quantity>) y)
      (let-values (((u) (unit x))
                   ((q r) (op (magnitude x) y)))
        (values (make-quantity q u)
                (make-quantity r u))))

    (define-method (op x (y <a-quantity>))
      (let-values (((u) (invert-unit y))
                   ((q r) (op x (magnitude y))))
        (values (make-quantity q u)
                (make-quantity r u))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method (* (x <a-quantity>) (y <a-quantity>))
  (let* ((ux (unit x))
         (uy (unit y))
         (d (* (dimension ux) (dimension uy)))
         (f (* (factor    ux) (factor    uy)))
         (v (* (magnitude  x) (magnitude  y))))
    (if (every zero? (exponents d))
        (* f v)
        (make-quantity v (get-unit f d)))))

(define-method (+ (x <a-quantity>) (y <a-quantity>))
  (let* ((u (unit x))
         (y (convert-to u y)))
    (make-quantity (+ (magnitude x) (magnitude y)) u)))

(define-method (- (x <a-quantity>) (y <a-quantity>))
  (let* ((u (unit x))
         (y (convert-to u y)))
    (make-quantity (- (magnitude x) (magnitude y)) u)))

(define-commutative-operator* *)
(define-commutative-operator* +)
(define-commutative-operator* -)
(define-divide-operator* /)

(define-method (> (x <a-quantity>) (y <a-quantity>))
  (positive? (magnitude (- x y))))

(define-method (< (x <a-quantity>) (y <a-quantity>))
  (negative? (magnitude (- x y))))

(define-method (= (x <a-quantity>) (y <a-quantity>))
  (zero? (magnitude (- x y))))

(define-commutative-predicate* >)
(define-commutative-predicate* <)
(define-commutative-predicate* =)

(define-method (nan? (x <a-quantity>))
  (nan? (magnitude x)))

(define-method (max x (y <a-quantity>))
  (if (> x y) x y))

(define-method (max (x <a-quantity>) y)
  (if (> x y) x y))

(define-method (min x (y <a-quantity>))
  (if (< x y) x y))

(define-method (min (x <a-quantity>) y)
  (if (< x y) x y))

(define-unary-operator* abs)
(define-unary-operator* truncate)
(define-unary-operator* round)
(define-unary-operator* floor)
(define-unary-operator* ceiling)

(define-divmod-operator* floor/)
(define-divide-operator* floor-quotient)
(define-divide-operator* floor-remainder)

(define-divmod-operator* ceiling/)
(define-divide-operator* ceiling-quotient)
(define-divide-operator* ceiling-remainder)

(define-divmod-operator* truncate/)
(define-divide-operator* truncate-quotient)
(define-divide-operator* truncate-remainder)

(define-divmod-operator* centered/)
(define-divide-operator* centered-quotient)
(define-divide-operator* centered-remainder)

(define-divmod-operator* round/)
(define-divide-operator* round-quotient)
(define-divide-operator* round-remainder)

(define-method (expt (x <a-quantity>) n)
  (let* ((u (unit x))
         (d (expt (dimension u) n))
         (f (expt (factor    u) n))
         (v (expt (magnitude x) n)))
    (make-quantity v (get-unit f d))))

(define-method (sqrt (x <a-quantity>))
  (let* ((u (unit x))
         (d (sqrt (dimension u)))
         (f (sqrt (factor    u)))
         (v (sqrt (magnitude x))))
    (make-quantity v (get-unit f d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-exponents names exponents)
  (string-join
   (filter (negate string-null?)
           (map (lambda (n e)
                  (cond ((eqv? e 0) "")
                        ((eqv? e 1) (symbol->string n))
                        (else (string-append
                               (symbol->string n)
                               (number->string e)))))
                names exponents))
   "."))

(define (base-dimensions-with-exponents d)
  (with-exponents (base-dimensions (unit-system d)) (exponents d)))

(define (base-units-with-exponents d)
  (with-exponents (base-unit-symbols (unit-system d)) (exponents d)))

(define-method (write (d <dimension>) p)
  (let* ((us (unit-system d))
         (is-base (member (name d) (base-dimensions us))))
    (display "#<dimension " p)
    (if is-base
        (display (name d) p)
        (begin
          (when (name d)
            (display (name d) p)
            (display #\= p))
          (display (base-dimensions-with-exponents d) p)))
    (display #\> p)))

(define-method (write (u <unit>) p)
  (let* ((d (dimension u))
         (us (unit-system d))
         (is-base (member (name u) (base-unit-names us))))
    (display "#<unit " p)
    (if is-base
        (begin
          (display (name d) p)
          (display #\space p)
          (display (name u) p)
          (display #\space p)
          (display (symbol u) p))
        (begin
          (when (name d)
            (display (name d) p)
            (display #\space p))
          (when (name u)
            (display (name u) p)
            (display #\space p))
          (when (symbol u)
            (display (symbol u) p)
            (display #\= p))
          (display (factor u) p)
          (display #\. p)
          (display (base-units-with-exponents d) p)))
    (display #\> p)))

(define-method (write (x <quantity>) p)
  (let* ((u (unit x))
         (d (dimension u)))
    (display "#<" p)
    (display (or (name d) "quantity") p)
    (display #\space p)
    (cond ((symbol u)
           (display (exact->inexact (magnitude x)) p)
           (display #\space p)
           (display (symbol u) p))
          ((name u)
           (display (exact->inexact (magnitude x)) p)
           (display #\space p)
           (display (name u) p))
          (else
           (display (exact->inexact (magnitude-in-base-units x)) p)
           (display #\space p)
           (display (base-units-with-exponents d) p)))
    (display #\> p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (combine-exponents dims exps)
  (reduce (lambda (exp1 exp2)
            (map + exp1 exp2))
          '()
          (map (lambda (dim x)
                 (map (cut * x <>)
                      (exponents dim)))
               dims exps)))

(define (deal2 lst)
  (let loop ((lst lst)
             (firsts '())
             (seconds '())
             (is-first #t))
    (if (null? lst)
        (values (reverse! firsts)
                (reverse! seconds))
        (let-values (((firsts seconds next)
                      (if is-first
                          (values (cons (car lst) firsts) seconds #f)
                          (values firsts (cons (car lst) seconds) #t))))
          (loop (cdr lst) firsts seconds next)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-unit-system
  (lambda (x)
    (syntax-case x ()
      ((_ us (dims units syms) ...)
       (let* ((dimensions   #'(dims  ...))
              (unit-names   #'(units ...))
              (unit-symbols #'(syms  ...))
              (exponents    (lambda (dim)
                              (map (lambda (d)
                                     (if (eq? dim d) 1 0))
                                   dimensions))))
         #`(begin

             (define us
               (make <unit-system>
                 #:name              'us
                 #:base-dimensions   '#,dimensions
                 #:base-unit-names   '#,unit-names
                 #:base-unit-symbols '#,unit-symbols))

             #,(let ((exp (make-list (length dimensions) 0)))
                 #`(make-dimension us '#,exp 'dimensionless))

             #,@(map (lambda (dim exp)
                       #`(define-dimension* us #,dim '#,exp))
                     dimensions (map exponents dimensions))

             #,@(map (lambda (dim unit sym)
                       #`(define-unit #,sym #,unit 1 #,dim))
                     dimensions unit-names unit-symbols)

             ))))))

(define-syntax define-dimension*
  (lambda (x)
    (syntax-case x ()
      ((_ us dim exp)
       (with-syntax
           ((dim-p (datum->syntax #'dim (symbol-append
                                         (syntax->datum #'dim)
                                         '?))))
         #`(begin
             (define dim
               (make-dimension us exp 'dim))
             (define (dim-p q)
               (compatible? dim (dimension q)))))))))

(define-syntax define-dimension
  (lambda (x)
    (syntax-case x ()
      ((_ dim (dims-and-exps ...))
       (let-values (((dims exps) (deal2 #'(dims-and-exps ...))))
         #`(begin
             (assert-same-unit-systems #,@dims)
             (define-dimension*
               (unit-system #,(first dims))
               dim
               (combine-exponents
                (list #,@dims)
                (list #,@exps))))))
      ((_ dim unit sym (dims-and-exps ...))
       #`(begin
           (define-dimension dim (dims-and-exps ...))
           (define-unit sym unit 1 dim))))))

(define-syntax define-dimensions
  (lambda (x)
    (syntax-case x ()
      ((_ (dims ...) ...)
       #`(begin
           #,@(map (lambda (dim)
                     #`(define-dimension #,@dim))
                   #'((dims ...) ...)))))))

(define-syntax define-unit
  (lambda (x)
    (syntax-case x ()
      ((_ sym unit fac dim)
       #'(define sym
           (make-unit fac dim 'unit 'sym)))
      ((_ sym unit quant)
       #'(define sym
           (as-unit quant 'unit 'sym))))))

(define-syntax define-units
  (lambda (x)
    (syntax-case x ()
      ((_ (units ...) ...)
       #`(begin
           #,@(map (lambda (unit)
                     #`(define-unit #,@unit))
                   #'((units ...) ...)))))))

(define-syntax define-prefixed-units
  (lambda (x)
    (syntax-case x ()
      ((_ (units ...) (syms prefs facs) ...)
       (let-values (((usyms unames) (deal2 #'(units ...))))
         #`(begin
             #,@(map (lambda (usym uname)
                       #`(begin
                           #,@(map (lambda (psym pname fac)
                                     (with-syntax ((sym
                                                    (datum->syntax usym (symbol-append
                                                                         (syntax->datum psym)
                                                                         (syntax->datum usym))))
                                                   (name
                                                    (datum->syntax uname (symbol-append
                                                                          (syntax->datum pname)
                                                                          (syntax->datum uname)))))
                                       #`(define-unit sym name (* #,fac #,usym))))
                                   #'(syms ...) #'(prefs ...) #'(facs ...))))
                     usyms unames)))))))
