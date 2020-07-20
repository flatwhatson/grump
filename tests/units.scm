(use-modules (grump units si)
             (srfi srfi-64))

(test-begin "units")

(test-group "dimension-arithmetic"
  (test-equal area     (* length length))
  (test-equal length   (/ area length))
  (test-equal length   (* velocity time))
  (test-equal force    (* mass acceleration))
  (test-equal pressure (/ force area))
  (test-error (+ length length))
  (test-error (- time area)))

(test-group "dimensions-of"
  (test-assert (length?           m))
  (test-assert (mass?             kg))
  (test-assert (velocity?         (/ m s)))
  (test-assert (electric-current? (/ C s)))
  (test-assert (energy?           (* N m)))
  (test-assert (pressure?         (/ N (* m m))))
  (test-assert (compatible? (/ energy volume) (/ J (* m m m)))))

(test-group "quantity-arithmetic"
  (test-equal (* m 50)   (+ (* m 20) (* m 30)))
  (test-equal (* m 5025) (+ (* km 5) (* m 25)))
  (test-equal (* J 2)    (* (* N 3) (* m 2/3)))
  (test-equal (* 2 J)    (* 2 N m))
  (test-equal (* Hz 2)   (+ (* Hz 1) (/ 1 s)))
  (test-equal (* Bq 2)   (+ (* Bq 1) (/ 1 s)))
  (test-error (+ (* m -3) (* s 5)))
  (test-error (- (* kg 4) (* J 3)))
  (test-error (+ (* Bq 2) (* Hz 3))))

(test-group "quantity-comparison"
  (test-assert (> (* m 20) (* m 10)))
  (test-assert (>= (* m 20) (* m 10)))
  (test-assert (< (* s 5) (* h 1)))
  (test-assert (<= (* kg 2) (* kg 2)))
  (test-assert (positive? (* s 3)))
  (test-assert (negative? (* s -3)))
  (test-assert (zero? (* s 0)))
  (test-error (< (* m -3) (* s 5)))
  (test-error (> (* kg 4) (* J 3))))

(test-group "math-functions"
  (test-equal 1           (sin (* deg 90)))
  (test-equal (* m 2)     (abs (* m 2)))
  (test-equal (* m 3)     (abs (* m -3)))
  ;(test-equal 1           (sgn (* s 2)))
  ;(test-equal 0           (sgn (* s 0)))
  ;(test-equal -1          (sgn (* s -1)))
  (test-equal (* 8 m m m) (expt (* m 2) 3))
  (test-equal (* m 1)     (sqrt (* m m)))
  (test-equal (/ pi 4)    (atan (* m 3) (* m 3))))

(test-end)
