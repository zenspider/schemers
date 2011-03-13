#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.27

;; Suppose we type in the following definitions to
;; the lazy evaluator:
;;
;;      (define count 0)
;;
;;      (define (id x)
;;        (set! count (+ count 1))
;;        x)
;;
;; Give the missing values in the following sequence of interactions,
;; and explain your answers.(5)
;;
;;      (define w (id (id 10)))
;;
;;      ;;; L-Eval input:
;;      count
;;      ;;; L-Eval value:
;;      <RESPONSE>
;;
;;      ;;; L-Eval input:
;;      w
;;      ;;; L-Eval value:
;;      <RESPONSE>
;;
;;      ;;; L-Eval input:
;;      count
;;      ;;; L-Eval value:
;;      <RESPONSE>
