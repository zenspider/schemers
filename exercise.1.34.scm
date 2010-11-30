#lang racket

;;; Exercise 1.34:

;; Suppose we define the procedure

(define (f g)
  (g 2))

;; Then we have

(define (square n) (* n n))
(f square)                              ; 4

(f (lambda (z) (* z (+ z 1))))          ; 6

;; What happens if we (perversely) ask the interpreter to evaluate
;; the combination `(f f)'?  Explain.

;;    (f f)

;; You get an error:

;; procedure application: expected procedure, given: 2; arguments were: 2

;; because:

;; (f f) => (f g=f) => (g=f 2) => (f g=2) => (2 2) => boom
