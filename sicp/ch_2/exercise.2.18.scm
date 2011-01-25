
;;; Exercise 2.18:

;; Define a procedure `reverse' that takes a list as argument and
;; returns a list of the same elements in reverse order:
;;
;;      (reverse (list 1 4 9 16 25))
;;      (25 16 9 4 1)

(require 'testes)
(import testes)

(define (reverse l)
  (define (iterate l r)
    (if (null? l) r
        (iterate (cdr l) (cons (car l) r))))
  (iterate l null))

(assert-equal '(3 2 1) (reverse '(1 2 3)))
(done)
