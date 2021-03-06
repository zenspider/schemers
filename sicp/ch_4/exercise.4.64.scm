#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 4.64

;; Louis Reasoner mistakenly deletes the
;; `outranked-by' rule (section *Note 4-4-1::) from the data base.
;; When he realizes this, he quickly reinstalls it.  Unfortunately,
;; he makes a slight change in the rule, and types it in as
;;
;;      (rule (outranked-by ?staff-person ?boss)
;;            (or (supervisor ?staff-person ?boss)
;;                (and (outranked-by ?middle-manager ?boss)
;;                     (supervisor ?staff-person ?middle-manager))))
;;
;; Just after Louis types this information into the system, DeWitt
;; Aull comes by to find out who outranks Ben Bitdiddle. He issues
;; the query
;;
;;      (outranked-by (Bitdiddle Ben) ?who)
;;
;; After answering, the system goes into an infinite loop.  Explain
;; why.

;; A: Easy answer: because louis is too stupid to use version control.
;;
;; A: Real answer: because the interpreter evaluates depth-first, it
;;    will evaluate the inner-outranked-by before it prunes w/
;;    supervisor. This causes an infinite loop because there are no
;;    bindings on middle-manager or boss at that time.
