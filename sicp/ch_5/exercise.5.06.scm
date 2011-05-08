#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.6

;; Ben Bitdiddle observes that the Fibonacci machine's controller
;; sequence has an extra `save' and an extra `restore', which can be
;; removed to make a faster machine. Where are these instructions?

;;  afterfib-n-1
;;    (restore n)
;;    (restore continue) ;; not necessary as we don't jump in-between
;;    (assign n (op -) (reg n) (const 2))
;;    (save continue)    ;; not necessary as we don't jump in-between
;;    (assign continue (label afterfib-n-2))
;;    (save val)
;;    (goto (label fib-loop))
