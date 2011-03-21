#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.18

;; Consider an alternative strategy for scanning out
;; definitions that translates the example in the text to
;;
;;      (lambda <VARS>
;;        (let ((u '*unassigned*)
;;              (v '*unassigned*))
;;          (let ((a <E1>)
;;                (b <E2>))
;;            (set! u a)
;;            (set! v b))
;;          <E3>))
;;
;; Here `a' and `b' are meant to represent new variable names, created
;; by the interpreter, that do not appear in the user's program.
;; Consider the `solve' procedure from section *Note 3-5-4:::
;;
;;      (define (solve f y0 dt)
;;        (define y (integral (delay dy) y0 dt))
;;        (define dy (stream-map f y))
;;        y)
;;
;; Will this procedure work if internal definitions are scanned out as
;; shown in this exercise?

;; A: Yes, it should work fine this way or with the letrec. They
;;    should be entirely equivalent.

;; What if they are scanned out as shown in the text? Explain.

;; A: AFAIK, that is entirely dependent upon the implementation of the
;; scheme you're using, as described in footnote 2.