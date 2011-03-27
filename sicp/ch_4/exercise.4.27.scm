#!/usr/bin/env csi -s

(use test)
(require-library lazy-eval)
(import lazy-eval)

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

(define env (setup-environment))
(test 'ok (eval '(define count 0) env))
(test 'ok (eval '(define (id x)
                   (set count (+ count 1))
                   x) env))
(test 'ok (eval '(define w (id (id 10))) env))

;; define w calls inner id, incrementing count

(test 1 (eval 'count env))
(test 10 (actual-value 'w env))         ; calls outer id, incrementing count
(test 2 (eval 'count env))

