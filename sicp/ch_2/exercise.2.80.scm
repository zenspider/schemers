
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 2.80

;; Define a generic predicate `=zero?' that tests if
;; its argument is zero, and install it in the generic arithmetic
;; package.  This operation should work for ordinary numbers, rational
;; numbers, and complex numbers.

(define (=zero? x) (= (magnitude x) 0))

(done)
