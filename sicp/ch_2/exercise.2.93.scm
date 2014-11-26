(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.93

;; Modify the rational-arithmetic package to use
;; generic operations, but change `make-rat' so that it does not
;; attempt to reduce fractions to lowest terms.  Test your system by
;; calling `make-rational' on two polynomials to produce a rational
;; function
;;
;;      (define p1 (make-polynomial 'x '((2 1)(0 1))))
;;      (define p2 (make-polynomial 'x '((3 1)(0 1))))
;;      (define rf (make-rational p2 p1))
;;
;; Now add `rf' to itself, using `add'. You will observe that this
;; addition procedure does not reduce fractions to lowest terms.
;;
;; We can reduce polynomial fractions to lowest terms using the same
;; idea we used with integers: modifying `make-rat' to divide both
;; the numerator and the denominator by their greatest common
;; divisor.  The notion of "greatest common divisor" makes sense for
;; polynomials.  In fact, we can compute the GCD of two polynomials
;; using essentially the same Euclid's Algorithm that works for
;; integers.(7)  The integer version is
;;
;;      (define (gcd a b)
;;        (if (= b 0)
;;            a
;;            (gcd b (remainder a b))))
;;
;; Using this, we could make the obvious modification to define a GCD
;; operation that works on term lists:
;;
;;      (define (gcd-terms a b)
;;        (if (empty-termlist? b)
;;            a
;;            (gcd-terms b (remainder-terms a b))))
;;
;; where `remainder-terms' picks out the remainder component of the
;; list returned by the term-list division operation `div-terms' that
;; was implemented in *Note Exercise 2-91::.

;; (assert-equal x y)
(done)
