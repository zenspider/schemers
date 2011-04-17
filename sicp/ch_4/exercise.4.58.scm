#!/usr/bin/env csi -s

(use test)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.58

;; Define a rule that says that a person is a "big shot" in a division
;; if the person works in the division but does not have a supervisor
;; who works in the division.

(assert! '(rule (bigshot ?p1 ?p2)
                (and (job ?p1 (?div1 . ?iggy1))
                     (or (not (supervisor ?p1 ?p2))
                         (and (supervisor ?p1 ?p2)
                              (not (job ?p2 (?div1 . ?iggy2))))))))

(test '((bigshot (Warbucks Oliver) ?p2-1)
        (bigshot (Scrooge Eben)  (Warbucks Oliver))
        (bigshot (Bitdiddle Ben) (Warbucks Oliver)))
      (all-of '(bigshot ?p1 ?p2)))

;; found on http://eli.thegreenplace.net/2008/02/08/sicp-section-441/
(assert! '(rule (bigshot2 ?p1 ?div)
                (and (job ?p1 (?div . ?iggy1))
                     (or (not (supervisor ?p1 ?p2))
                         (and (supervisor ?p1 ?p2)
                              (not (job ?p2 (?div . ?iggy2))))))))

(test '((bigshot2 (Warbucks Oliver) administration)
        (bigshot2 (Scrooge Eben) accounting)
        (bigshot2 (Bitdiddle Ben) computer))
      (all-of '(bigshot2 ?p1 ?p2)))

