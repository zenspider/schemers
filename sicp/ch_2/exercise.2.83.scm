
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)
(require 'table)
(import table)
;; (require "../../lib/table.rkt")

;;; Exercise 2.83

;; Suppose you are designing a generic arithmetic
;; system for dealing with the tower of types shown in *Note Figure
;; 2-25::: integer, rational, real, complex.  For each type (except
;; complex), design a procedure that raises objects of that type one
;; level in the tower.  Show how to install a generic `raise'
;; operation that will work for each type (except complex).

;; stolen from aaron
(define coercion-table (make-table))
(define (put-coerce from to proc)
  (insert! (cons from to) proc coercion-table))
(define (get-coerce from to)
  (lookup (cons from to) coercion-table))

(define tower '(integer rational real complex))

;; (define (integer->rational n)
;;   (make-rational n 1))
;;
;; (define (rational->real n) (/ (numer n) (denom n)))
;;
;; (define (real->complex n)
;;   (make-complex-from-real-imag n 0))
;;
;; ;; I don't like the idea of going from rational to real to complex
;; (define (rational->complex n)
;;   (make-complex-from-real-imag n 0))

;; (define (raise x)
;;   (let ((from (tag-type x)))
;;     (let ((rest (member x tower)))
;;       (if (or (null? rest) (null? (cdr rest)))
;;           (error "no raise exists for type" (list x from))
;;           ((get-coersion from (cadr rest)) x)))))

;; (put-coerce integer rational integer->rational)
;; (put-coerce rational real    rational->real)
;; (put-coerce real complex     real->complex)

(assert-equal '(rational real complex) (member 'rational tower))
(assert-equal 'real (cadr (member 'rational tower)))

;; (assert-equal x y)
(done)
