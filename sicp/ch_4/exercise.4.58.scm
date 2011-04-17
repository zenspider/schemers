#!/usr/bin/env csi -s

(use test)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.58

;; Define a rule that says that a person is a "big shot" in a division
;; if the person works in the division but does not have a supervisor
;; who works in the division.

(pp (all-of '(and (job ?p1 (?div1 . ?iggy1))
                  (supervisor ?p1 ?p2)
                  (job ?p2 (?div2 . ?iggy2))
                  (not (same ?div1 ?div2)))))

(assert! '(rule (bigshot ?p1 ?p2)
                (and (job ?p1 (?div1 . ?iggy1))
                     (supervisor ?p1 ?p2)
                     (job ?p2 (?div2 . ?iggy2))
                     (not (same ?div1 ?div2)))))

(test '((bigshot (Scrooge Eben)  (Warbucks Oliver))
        (bigshot (Bitdiddle Ben) (Warbucks Oliver)))
      (all-of '(bigshot ?p1 ?p2)))
