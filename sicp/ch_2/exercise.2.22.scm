#lang racket

(require "../lib/testes.rkt")

;;; Exercise 2.22:

;; Louis Reasoner tries to rewrite the first `square-list' procedure
;; of *Note Exercise 2-21:: so that it evolves an iterative process:

(define (square n) (* n n))

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

;; Unfortunately, defining `square-list' this way produces the answer
;; list in the reverse order of the one desired. Why?
;; 
;; Louis then tries to fix his bug by interchanging the arguments to
;; `cons':

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

;; This doesn't work either.  Explain.

;; because he writes scheme code like I do, according to my previous
;; assignments.

(assert-equal '(16 9 4 1)                  (square-list1 '(1 2 3 4)))
(assert-equal '((((() . 1) . 4) . 9) . 16) (square-list2 '(1 2 3 4)))
(done)
