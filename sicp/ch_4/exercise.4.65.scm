#!/usr/bin/env csi -s

(require rackunit)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.65

;; Cy D. Fect, looking forward to the day when he
;; will rise in the organization, gives a query to find all the
;; wheels (using the `wheel' rule of section *Note 4-4-1::):
;;
;;      (wheel ?who)
;;
;; To his surprise, the system responds
;;
;;      ;;; Query results:
;;      (wheel (Warbucks Oliver))
;;      (wheel (Bitdiddle Ben))
;;      (wheel (Warbucks Oliver))
;;      (wheel (Warbucks Oliver))
;;      (wheel (Warbucks Oliver))
;;
;; Why is Oliver Warbucks listed four times?

(assert! '(rule (wheel ?person)
                (and (supervisor ?middle-manager ?person)
                     (supervisor ?x ?middle-manager))))

;; Oddly, I get it not 4, but 8 times. I suspect something is wrong
;; with the implementation that we're using, but don't care enough to
;; figure out why.

(test '((wheel (Warbucks Oliver))
        (wheel (Warbucks Oliver))
        (wheel (Warbucks Oliver))
        (wheel (Warbucks Oliver))
        (wheel (Bitdiddle Ben))
        (wheel (Bitdiddle Ben))
        (wheel (Warbucks Oliver))
        (wheel (Warbucks Oliver))
        (wheel (Warbucks Oliver))
        (wheel (Warbucks Oliver)))
      (all-of '(wheel ?who)))

;; If I rewrite the rule with arity 2, I get:

(assert! '(rule (wheel2 ?who ?person)
                (and (supervisor ?middle-manager ?person)
                     (supervisor ?who ?middle-manager))))

(test '((wheel2 (Cratchet Robert) (Warbucks Oliver))
        (wheel2 (Tweakit Lem E)   (Warbucks Oliver))
        (wheel2 (Reasoner Louis)  (Bitdiddle Ben))
        (wheel2 (Fect Cy D)       (Warbucks Oliver))
        (wheel2 (Hacker Alyssa P) (Warbucks Oliver)))
      (all-of '(wheel2 ?who ?person)))

;; so at this point I think the duplicates from above are due to not
;; having a same rule in there.... nope. not the reason. *shrug*

;; So the reason you get the 4 Warbucks is because of the structure of
;; the organization coupled with how the interpreter works:

(test '((supervisor (Aull DeWitt)   (Warbucks Oliver))
        (supervisor (Scrooge Eben)  (Warbucks Oliver))
        (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
      (all-of '(supervisor ?who (Warbucks Oliver))))

;; =

(test '((supervisor (Tweakit Lem E)   (Bitdiddle Ben))
        (supervisor (Fect Cy D)       (Bitdiddle Ben))
        (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
      (all-of '(supervisor ?x (Bitdiddle Ben))))

;; +

(test '((supervisor (Cratchet Robert) (Scrooge Eben)))
      (all-of '(supervisor ?x (Scrooge Eben))))

;; the interpreter finds ALL matches, not just unique matches.
