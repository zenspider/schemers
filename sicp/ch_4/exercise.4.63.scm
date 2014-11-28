#!/usr/bin/env csi -s

(require rackunit)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.63

;; The following data base (see Genesis 4) traces
;; the genealogy of the descendants of Ada back to Adam, by way of
;; Cain:

(assert! '(son Adam Cain))
(assert! '(son Cain Enoch))
(assert! '(son Enoch Irad))
(assert! '(son Irad Mehujael))
(assert! '(son Mehujael Methushael))
(assert! '(son Methushael Lamech))
(assert! '(wife Lamech Ada))
(assert! '(son Ada Jabal))
(assert! '(son Ada Jubal))

;; Formulate rules such as "If S is the son of f, and f is the son of
;; G, then S is the grandson of G" and "If W is the wife of M, and S
;; is the son of W, then S is the son of M" (which was supposedly
;; more true in biblical times than today) that will enable the query
;; system to find the grandson of Cain; the sons of Lamech; the
;; grandsons of Methushael.  (See *Note Exercise 4-69:: for some
;; rules to deduce more complicated relationships.)

(assert! '(rule (grandson ?x ?z)
                (and (son ?x ?y)
                     (son ?y ?z))))

;; If W is the wife of M, and S is the son of W, then S is the son of M
(assert! '(rule (son ?f ?s)
                (and (wife ?f ?m)
                     (son  ?m ?s))))

(test '((grandson Cain Irad))
      (all-of '(grandson Cain ?z)))

(test '((son Lamech Jubal)
        (son Lamech Jabal))
      (all-of '(son Lamech ?x)))

(test '((grandson Methushael Jubal)
        (grandson Methushael Jabal))
      (all-of '(grandson Methushael ?x)))