#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.79

;; Define a generic equality predicate `equ?' that
;; tests the equality of two numbers, and install it in the generic
;; arithmetic package.  This operation should work for ordinary
;; numbers, rational numbers, and complex numbers.

;; I read this problem as "how do you define this equality operation
;; generically enough that it works for all number systems?"

;; (define (equ? x y)
;;   (and (= (real-part x) (real-part y))
;;        (= (imag-part x) (imag-part y))))

;; this requires all base type packages to define real-part and
;; imag-part, which is defined globally on page 184 in section 2.4.3.
;;
;; it will also answer true for (equ? 4 (make-rational 4 0))
;;
;; it will also not need extending every time a new package is added,
;; provided that the new package upholds the global contract that
;; real-part and imag-part are defined.

;; vs:

;; (put 'equ? '(scheme-number scheme-number)
;;      (lambda (x y)
;;        (= x y)))
;; (put 'equ? '(rational rational)
;;      (lambda (x y)
;;        (and (= (numer x) (numer y))
;;             (= (denom x) (denom y)))))
;; (put 'equ? '(complex complex)
;;      (lambda (x y)
;;        (and (= (real-part x) (real-part y))
;;             (= (imag-part x) (imag-part y)))))
;; (put 'equ? '(polar polar)
;;      (lambda (x y)
;;        (and (= (magnitude x) (magnitude y))
;;             (= (angle x) (angle y)))))
;; and so on
