#lang racket/base

(require "../lib/testes.scm")
(require "../lib/myutils.scm")
(require rnrs/mutable-pairs-6)

;;; Exercise 3.15

;; Draw box-and-pointer diagrams to explain the
;; effect of `set-to-wow!' on the structures `z1' and `z2' above.

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))

(assert-equal '((a b) a b) z1)

;; z1 = *
;;      |\
;;      |/
;;      *--*--0
;;      |  |
;;      a  b

(assert-equal '((wow b) wow b) (set-to-wow! z1))

;; z1 = *
;;      |\
;;      |/
;;      *---*--0
;;      |   |
;;      wow b

;; z2 = *--*--*--0
;;      |  |  |
;;      |  a  b
;;      |  |  |
;;      +--*--*--0

(assert-equal '((a b) a b) z2)

;; z2 = *--*--*--0
;;      |  |  |
;;      |  a  b
;;      |     |
;;      +--*--*--0
;;         |
;;         wow

(assert-equal '((wow b) a b) (set-to-wow! z2))

(done)
