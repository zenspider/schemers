#!/usr/bin/env csi -s

(require rackunit)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.61

;; The following rules implement a `next-to'
;; relation that finds adjacent elements of a list:

(assert! '(rule (?x next-to ?y in (?x ?y . ?u))))

(assert! '(rule (?x next-to ?y in (?v . ?z))
                (?x next-to ?y in ?z)))

;; What will the response be to the following queries?
;;
;;      (?x next-to ?y in (1 (2 3) 4))

;; should respond with 1 next to (2 3), (2 3) next to 4.

;;      (?x next-to 1 in (2 1 3 1))

;; should respond with 2 next to 1, 3 next to 1

(test '(((2 3) next-to 4 in (1 (2 3) 4))
        (1 next-to (2 3) in (1 (2 3) 4)))
      (all-of '(?x next-to ?y in (1 (2 3) 4))))

(test '((3 next-to 1 in (2 1 3 1))
        (2 next-to 1 in (2 1 3 1)))
      (all-of '(?x next-to 1 in (2 1 3 1))))
