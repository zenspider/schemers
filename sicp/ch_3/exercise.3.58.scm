#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.58

;; Give an interpretation of the stream computed by
;; the following procedure:
;;
;;      (define (expand num den radix)
;;        (cons-stream
;;         (quotient (* num radix) den)
;;         (expand (remainder (* num radix) den) den radix)))
;;
;; (`Quotient' is a primitive that returns the integer quotient of two
;; integers.)  What are the successive elements produced by `(expand
;; 1 7 10)'?  What is produced by `(expand 3 8 10)'?

;; (assert-equal x y)
(done)
