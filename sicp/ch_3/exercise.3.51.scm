#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.51

;; In order to take a closer look at delayed
;; evaluation, we will use the following procedure, which simply
;; returns its argument after printing it:
;;
;;      (define (show x)
;;        (display-line x)
;;        x)
;;
;; What does the interpreter print in response to evaluating each
;; expression in the following sequence?(7)
;;
;;      (define x (stream-map show (stream-enumerate-interval 0 10)))
;;
;;      (stream-ref x 5)
;;
;;      (stream-ref x 7)

;; (assert-equal x y)
(done)
