#lang racket/base

(require "../lib/test.rkt")
(require "../lib/constraints.scm")

;;; Exercise 3.37

;; The `celsius-fahrenheit-converter' procedure is
;; cumbersome when compared with a more expression-oriented style of
;; definition, such as
;;
;;      (define (celsius-fahrenheit-converter x)
;;        (c+ (c* (c/ (cv 9) (cv 5))
;;                x)
;;            (cv 32)))
;;
;;      (define C (make-connector))
;;      (define F (celsius-fahrenheit-converter C))
;;
;; Here `c+', `c*', etc. are the "constraint" versions of the
;; arithmetic operations.  For example, `c+' takes two connectors as
;; arguments and returns a connector that is related to these by an
;; adder constraint:

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;; Define analogous procedures `c-', `c*', `c/', and `cv' (constant
;; value) that enable us to define compound constraints as in the
;; converter example above.(3)

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv val)
  (let ((temp (make-connector)))
    (set-value! temp val 'user)
    temp))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(test-group "3.37"
            (set-value! F 212 'user)
            (test 100.0 (get-value C)))
