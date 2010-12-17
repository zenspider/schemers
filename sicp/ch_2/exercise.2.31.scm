#lang racket

(require "../lib/testes.rkt")

;; Exercise 2.31.

;; Abstract your answer to exercise 2.30 to produce a procedure
;; tree-map with the property that square-tree could be defined as
;;
;; (define (square-tree tree) (tree-map square tree))

(define (square n) (* n n))

(define (tree-map f l)
  (map (lambda (x) (if (list? x) (tree-map f x) (f x))) l))

(define (square-tree tree) (tree-map square tree))

(assert-equal '()    (square-tree '()))
(assert-equal '(4)   (square-tree '(2)))
(assert-equal '(4 9) (square-tree '(2 3)))
(assert-equal '(1 (4 (9 16) 25) (36 49)) (square-tree '(1 (2 (3 4) 5) (6 7))))
(done)
