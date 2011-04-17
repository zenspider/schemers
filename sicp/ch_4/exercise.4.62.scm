#!/usr/bin/env csi -s

(use test)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.62

;; Define rules to implement the `last-pair' operation of *Note
;; Exercise 2-17::, which returns a list containing the last element
;; of a nonempty list. Check your rules on queries such as `(last-pair
;; (3) ?x)', `(last-pair (1 2 3) ?x)', and `(last-pair (2 ?x) (3))'.
;; Do your rules work correctly on queries such as `(last-pair ?x
;; (3))' ?

(assert! '(rule (last-pair (?x) (?x))))
(assert! '(rule (last-pair (?x . ?l1) (?l2))
                (last-pair ?l1 (?l2))))

(test '((last-pair     (3) (3))) (all-of '(last-pair (3)     ?x)))
(test '((last-pair (1 2 3) (3))) (all-of '(last-pair (1 2 3) ?x)))
(test '((last-pair   (2 3) (3))) (all-of '(last-pair (2 ?x)  (3))))

;; presumably takes an ∞ amount of time
;; (test '((last-pair (3)     (3))) (all-of '(last-pair ?x      (3))))