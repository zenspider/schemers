#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.28

;; Modify the definition of the evaluator by
;; changing `eval-sequence' as described in section *Note 5-4-2:: so
;; that the evaluator is no longer tail-recursive.  Rerun your
;; experiments from *Note Exercise 5-26:: and *Note Exercise 5-27::
;; to demonstrate that both versions of the `factorial' procedure now
;; require space that grows linearly with their input.
