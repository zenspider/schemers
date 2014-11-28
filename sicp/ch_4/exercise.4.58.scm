#!/usr/bin/env csi -s

(require rackunit)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.58

;; Define a rule that says that a person is a "big shot" in a division
;; if the person works in the division but does not have a supervisor
;; who works in the division.

(assert! '(rule (bigshot ?who ?where)
                (and (job ?who (?where . ?iggy1))
                     (or (not (supervisor ?who ?p2))
                         (and (supervisor ?who ?p2)
                              (not (job ?p2 (?where . ?iggy2))))))))

(test '((bigshot (Warbucks Oliver) administration)
        (bigshot (Scrooge Eben)    accounting)
        (bigshot (Bitdiddle Ben)   computer))
      (all-of '(bigshot ?who ?where)))
