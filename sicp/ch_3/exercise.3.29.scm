#lang racket/base

(require (only-in "../lib/circuits.scm" make-wire inverter and-gate))

;;; Exercise 3.29

;; Another way to construct an or-gate is as a compound digital logic
;; device, built from and-gates and inverters. Define a procedure
;; `or-gate' that accomplishes this.

;; A B A|B
;; 0 0 0
;; 0 1 1
;; 1 0 1
;; 1 1 1
;;
;; or: !(!A & !B)
;; or: ->-|\
;;        | )->-
;;     ->-|/

(define (or-gate a b output)
  (let ((c (make-wire))
        (d (make-wire))
        (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e output)))

;; What is the delay time of the or-gate in terms of `and-gate-delay'
;; and `inverter-delay'?

;; A: 2i + a. See diagram above for obvious reason.
