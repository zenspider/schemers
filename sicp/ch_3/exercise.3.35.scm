#lang racket/base

(require "../lib/test.rkt")
(require "../lib/constraints.scm")

;;; Exercise 3.35

;; Ben Bitdiddle tells Louis that one way to avoid
;; the trouble in *Note Exercise 3-34:: is to define a squarer as a
;; new primitive constraint.  Fill in the missing portions in Ben's
;; outline for a procedure to implement such a constraint:
;;
;;      (define (squarer a b)
;;        (define (process-new-value)
;;          (if (has-value? b)
;;              (if (< (get-value b) 0)
;;                  (error "square less than 0 -- SQUARER" (get-value b))
;;                  <ALTERNATIVE1>)
;;              <ALTERNATIVE2>))
;;        (define (process-forget-value) <BODY1>)
;;        (define (me request) <BODY2>)
;;        <REST OF DEFINITION>
;;        me)

(define (squarer a aa)
  (define (process-new-value)
    (if (has-value? aa)
        (if (< (get-value aa) 0)
            (error "square less than 0 -- SQUARER" (get-value aa))
            (set-value! a (sqrt (get-value aa)) me))
        (set-value! aa (expt (get-value a) 2) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! aa me))
  (define (me m)
    (cond ((eq? m 'I-have-a-value)  (process-new-value))
          ((eq? m 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- SQUARER" m))))
  (connect a me)
  (connect aa me)
  me)

(test-group "3.35"
            (define A (make-connector))
            (define B (make-connector))
            (define C (make-connector))

            (squarer A B)
            (squarer B C)

            ;; Honestly... I'm not seeing it.

            (test/error (set-value! B -1 'user))

            (set-value! A 2 'user)
            (test  4 (get-value B))
            (test 16 (get-value C))

            (forget-value! A 'user)
            (forget-value! B 'user)
            (forget-value! C 'user)

            (set-value! C 16 'user)
            (test 4.0 (get-value B))
            (test 2.0 (get-value A)))
