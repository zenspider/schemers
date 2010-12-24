#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;; Exercise 2.39

;; Complete the following definitions of `reverse' (*Note Exercise
;; 2-18::) in terms of `fold-right' and `fold-left' from *Note
;; Exercise 2-38:::

;;      (define (reverse sequence)
;;        (fold-right (lambda (x y) <??>) nil sequence))
;;
;;      (define (reverse sequence)
;;        (fold-left (lambda (x y) <??>) nil sequence))

(define (reverse1 sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse2 sequence)
  (fold-left  (lambda (x y) (append (list y) x)) null sequence))

(assert-many (lambda (f)
               (assert-equal '(3 2 1) (f '(1 2 3))))
             reverse1
             reverse2)
(done)
