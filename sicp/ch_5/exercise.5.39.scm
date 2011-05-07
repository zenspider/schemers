#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.39

;; Write a procedure `lexical-address-lookup' that
;; implements the new lookup operation.  It should take two
;; arguments--a lexical address and a run-time environment--and
;; return the value of the variable stored at the specified lexical
;; address.  `Lexical-address-lookup' should signal an error if the
;; value of the variable is the symbol `*unassigned*'.(2) Also write
;; a procedure `lexical-address-set!' that implements the operation
;; that changes the value of the variable at a specified lexical
;; address.
