#!/usr/bin/env csi -s

(use test)
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

(define-rule '(same ?x ?x))

(define-rule '(outranked-by ?staff-person ?boss)
  '(or      (supervisor ?staff-person ?boss)
       (and (supervisor ?staff-person ?middle-manager)
            (outranked-by ?middle-manager ?boss))))

(define-rule '(wtf ?p1)
  '(job ?p1 ?x))

;; passes
(test '((outranked-by (Fect Cy D) (Bitdiddle Ben))
        (outranked-by (Fect Cy D) (Warbucks Oliver)))
      (all-of '(outranked-by (Fect Cy D) ?boss)))

;; passes
(test '((and (job (Hacker Alyssa P) (computer programmer))
             (not (same (Fect Cy D) (Hacker Alyssa P)))))
      (all-of '(and (job ?p2 (computer programmer))
                    (not (same (Fect Cy D) ?p2)))))

;; fails:
(test (all-of '(job (Fect Cy D) ?job))
      (all-of '(wtf (Fect Cy D))))

;; fails:
(test (all-of '(and (job ?p2 (computer programmer))
                    (not (same (Fect Cy D) ?p2))))
      (all-of '(same-job ?p (Fect Cy D))))
