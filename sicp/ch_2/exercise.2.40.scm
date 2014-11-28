#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; Exercise 2.40

;; Define a procedure `unique-pairs' that, given an integer n,
;; generates the sequence of pairs (i,j) with 1 <= j< i <= n. Use
;; `unique-pairs' to simplify the definition of `prime-sum-pairs'
;; given above.

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(assert-equal '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7)) (prime-sum-pairs 5))

;; (assert-equal x y)
(done)
