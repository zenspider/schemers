#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 2.56:

;; Show how to extend the basic differentiator to handle more kinds of
;; expressions. For instance, implement the differentiation rule
;;
;;      d(u^n)            du
;;      ---- = nu^(n-1) * --
;;        dx              dx
;;
;; by adding a new clause to the `deriv' program and defining
;; appropriate procedures `exponentiation?', `base', `exponent', and
;; `make-exponentiation'.  (You may use the symbol `**' to denote
;; exponentiation.)  Build in the rules that anything raised to the
;; power 0 is 1 and anything raised to the power 1 is the thing
;; itself.

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (operator? x op)
  (and (pair? x) (eq? (car x) op)))

(define (sum? x)
  (operator? x '+))

(define addend cadr)
(define augend caddr)

(define (product? x)
  (operator? x '*))

(define multiplier cadr)
(define multiplicand caddr)

(define (exponentiation? x)
  (operator? x '**))
(define base cadr)
(define exponent caddr)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(assert-equal 1 (deriv '(+ x 3) 'x))
(assert-equal 'y (deriv '(* x y) 'x))
(assert-equal '(+ (* x y) (* y (+  x 3)))
              (deriv '(* (* x y) (+ x 3)) 'x))

(assert-equal #t (exponentiation? '(** x 1)))
(assert-equal 'x (base '(** x 1)))
(assert-equal 1  (exponent '(** x 1)))

(assert-equal 1               (deriv '(** x 1) 'x))
(assert-equal 0               (deriv '(** x 0) 'x))
(assert-equal 0               (deriv '(** y 1) 'x))
(assert-equal 0               (deriv '(** y 0) 'x))
(assert-equal '(* 3 (** x 2)) (deriv '(** x 3) 'x))

(done)
