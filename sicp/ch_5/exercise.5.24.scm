#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.24

;; Implement `cond' as a new basic special form
;; without reducing it to `if'.  You will have to construct a loop
;; that tests the predicates of successive `cond' clauses until you
;; find one that is true, and then use `ev-sequence' to evaluate the
;; actions of the clause.
