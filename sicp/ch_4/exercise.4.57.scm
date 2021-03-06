#!/usr/bin/env csi -s

(require rackunit)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.57

;; Define a rule that says that person 1 can replace person 2 if
;; either person 1 does the same job as person 2 or someone who does
;; person 1's job can also do person 2's job, and if person 1 and
;; person 2 are not the same person. Using your rule, give queries
;; that find the following:
;;
;;   a. all people who can replace Cy D. Fect;
;;
;;   b. all people who can replace someone who is being paid more
;;      than they are, together with the two salaries.

(assert! '(rule (can-replace ?p1 ?p2)
                (and (or (and (job ?p1 ?x)
                              (job ?p2 ?x))
                         (and (job ?p1 ?x)
                              (job ?p2 ?y)
                              (can-do-job ?x ?y)))
                     (not (same ?p1 ?p2)))))

(test '((can-replace (Bitdiddle Ben)   (Fect Cy D))
        (can-replace (Hacker Alyssa P) (Fect Cy D)))
      (all-of '(can-replace ?p (Fect Cy D))))
