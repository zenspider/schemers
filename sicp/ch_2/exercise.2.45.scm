#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; (require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Exercise 2.45

;; `Right-split' and `up-split' can be expressed as instances of a
;; general splitting operation. Define a procedure `split' with the
;; property that evaluating
;;
;;      (define right-split (split beside below))
;;      (define up-split (split below beside))
;;
;; produces procedures `right-split' and `up-split' with the same
;; behaviors as the ones already defined.

(define (split x y)
  (define (f painter n)
    (if (= n 0) painter
        (let ((smaller (f painter (- n 1))))
          (x painter (y smaller smaller)))))
  f)

;; (define right-split (split beside below))
;; (define up-split (split below beside))

;; (paint (up-split einstein 4))
;; (paint (right-split einstein 4))
