#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.3

;; Rewrite `eval' so that the dispatch is done in
;; data-directed style.  Compare this with the data-directed
;; differentiation procedure of *Note Exercise 2-73::.  (You may use
;; the `car' of a compound expression as the type of the expression,
;; as is appropriate for the syntax implemented in this section.)
