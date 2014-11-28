#!/usr/bin/env csi -s

(require rackunit)
(use amb amb-extras)

;;; Exercise 4.39

;; Does the order of the restrictions in the multiple-dwelling
;; procedure affect the answer? Does it affect the time to find an
;; answer? If you think it matters, demonstrate a faster program
;; obtained from the given one by reordering the restrictions. If you
;; think it does not matter, argue your case.

;; Answered in seminar :P

;; Yes, it matters very much. The more reductions you can do up front
;; the better. A classic example would be the evens test in the sieve
;; of Eratosthenes, if that came after other checks (eg, if you tested
;; against known primes in descending order) you'd do a LOT more work.

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))

    (amb-assert (distinct? (list baker cooper fletcher miller smith))) ; 1
    (amb-assert (not (= (abs (- smith fletcher)) 1)))                  ; 7
    (amb-assert (not (= (abs (- fletcher cooper)) 1)))                 ; 8
    (amb-assert (> miller cooper))                                     ; 6
    (amb-assert (not (= baker 5)))                                     ; 2
    (amb-assert (not (= cooper 1)))                                    ; 3
    (amb-assert (not (= fletcher 5)))                                  ; 4
    (amb-assert (not (= fletcher 1)))                                  ; 4

    (list (list 'baker    baker)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

;; original = 1716 checks
;; above    = 1614 checks

(test '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
      (all-of (multiple-dwelling)))
