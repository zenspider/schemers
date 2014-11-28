#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.55:

;; Eva Lu Ator types to the interpreter the expression
;;
;;      (car ''abracadabra)
;;
;; To her surprise, the interpreter prints back `quote'.  Explain.

(assert-equal ''abracadabra  (quote (quote abracadabra)))
(assert-equal 'quote         (car (quote (quote abracadabra))))
(assert-equal '(abracadabra) (cdr (quote (quote abracadabra))))
(done)
