
(use test)
(require 'constraints)
(import constraints)

;;; Exercise 3.34

;; Louis Reasoner wants to build a squarer, a
;; constraint device with two terminals such that the value of
;; connector `b' on the second terminal will always be the square of
;; the value `a' on the first terminal.  He proposes the following
;; simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

;; There is a serious flaw in this idea.  Explain.

(test-group "3.34"
            (define A (make-connector))
            (define B (make-connector))
            (define C (make-connector))

            (squarer A B)
            (squarer B C)

            ;; Honestly... I'm not seeing it.

            (set-value! A 2 'user)
            (test  4 (get-value B))
            (test 16 (get-value C))

            (forget-value! A 'user)
            (forget-value! B 'user)
            (forget-value! C 'user)

            (set-value! C 16 'user)
            (test 4 (get-value B))
            (test 2 (get-value A)))

