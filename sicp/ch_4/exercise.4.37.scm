#!/usr/bin/env csi -s

(require rackunit)
(use amb amb-extras)

;;; Exercise 4.37

;; Ben Bitdiddle claims that the following method
;; for generating Pythagorean triples is more efficient than the one
;; in *Note Exercise 4-35::.  Is he correct?  (Hint: Consider the
;; number of possibilities that must be explored.)

(define (an-integer-between low high)
  (amb-assert (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (amb-assert (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (amb-assert (integer? k))
          (list i j k))))))

;; (all-of (pythagorean-triples 5))            ; 15 tries
;; (all-of (a-pythagorean-triple-between 1 5)) ; 9 tries
