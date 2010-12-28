#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 2.60:

;; We specified that a set would be represented as a list with no
;; duplicates. Now suppose we allow duplicates. For instance, the set
;; {1,2,3} could be represented as the list `(2 3 2 1 3 2 2)'. Design
;; procedures `element-of-set?', `adjoin-set', `union-set', and
;; `intersection-set' that operate on this representation. How does
;; the efficiency of each compare with the corresponding procedure for
;; the non-duplicate representation? Are there applications for which
;; you would use this representation in preference to the
;; non-duplicate one?

;; (assert-equal x y)
(done)
