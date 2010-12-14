#lang racket

(require "../lib/testes.rkt")

;; Exercise 2.27.  

;; Modify your reverse procedure of exercise 2.18 to produce a
;; deep-reverse procedure that takes a list as argument and returns as
;; its value the list with its elements reversed and with all sublists
;; deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))
;;
;; (reverse x)
;; ((3 4) (1 2))
;;
;; (deep-reverse x)
;; ((4 3) (2 1))

(define (reverse l)
  (define (iterate l r)
    (if (null? l) r
        (iterate (cdr l) (cons (car l) r))))
  (iterate l null))

(define (deep-reverse l)
  (define (iterate l r)
    (cond ((null? l) r)
          ((pair? (car l)) (iterate (cdr l) (cons (deep-reverse (car l)) r)))
          (else (iterate (cdr l) (cons (car l) r)))))
  (iterate l null))

(assert-equal '((3 4) (1 2)) (reverse x))
(assert-equal '((4 3) (2 1)) (deep-reverse x))


(assert-equal '((6 5 (4 3)) (2 1)) (deep-reverse '((1 2) ((3 4) 5 6))))
