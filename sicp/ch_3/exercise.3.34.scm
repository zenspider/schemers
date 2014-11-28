#lang racket/base

(require rackunit)
(require "../lib/constraints.scm")

;;; Exercise 3.34

;; Louis Reasoner wants to build a squarer, a
;; constraint device with two terminals such that the value of
;; connector `b' on the second terminal will always be the square of
;; the value `a' on the first terminal.  He proposes the following
;; simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

;; There is a serious flaw in this idea.  Explain.

(test-case "3.34"
           (define A (make-connector))
           (define B (make-connector))
           (define C (make-connector))

           (squarer A B)
           (squarer B C)

           ;; Honestly... I'm not seeing it.

           (set-value! A 2 'user)
           (check-equal?  (get-value B)
                          4)
           (check-equal? (get-value C)
                         16)

           (forget-value! A 'user)
           (forget-value! B 'user)
           (forget-value! C 'user)

           (set-value! C 16 'user)
           (check-equal? (get-value B)
                         4)
           (check-equal? (get-value A)
                         2))
