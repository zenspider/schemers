#!/usr/bin/env csi -s

(require rackunit)
(use amb amb-extras)

;;; Exercise 4.38

;; Modify the multiple-dwelling procedure to omit
;; the requirement that Smith and Fletcher do not live on adjacent
;; floors.  How many solutions are there to this modified puzzle?

(define (orig-multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (amb-assert (distinct? (list baker cooper fletcher miller smith)))
    (amb-assert (not (= baker 5)))
    (amb-assert (not (= cooper 1)))
    (amb-assert (not (= fletcher 5)))
    (amb-assert (not (= fletcher 1)))
    (amb-assert (> miller cooper))
    (amb-assert (not (= (abs (- smith fletcher)) 1)))
    (amb-assert (not (= (abs (- fletcher cooper)) 1)))

    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(test '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
      (all-of (orig-multiple-dwelling)))

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (amb-assert (distinct? (list baker cooper fletcher miller smith)))
    (amb-assert (not (= baker 5)))
    (amb-assert (not (= cooper 1)))
    (amb-assert (not (= fletcher 5)))
    (amb-assert (not (= fletcher 1)))
    (amb-assert (> miller cooper))
    (amb-assert (not (= (abs (- fletcher cooper)) 1)))

    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(test '(((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
        ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
        ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
        ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
        ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1)))
      (all-of (multiple-dwelling)))
