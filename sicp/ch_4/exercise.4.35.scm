#!/usr/bin/env csi -s

(use test amb amb-extras)

;;; Exercise 4.35

;; Write a procedure `an-integer-between' that returns an integer
;; between two given bounds. This can be used to implement a procedure
;; that finds Pythagorean triples, i.e., triples of integers (i,j,k)
;; between the given bounds such that i <= j and i^2 + j^2 = k^2, as
;; follows:
;;
;;      (define (a-pythagorean-triple-between low high)
;;        (let ((i (an-integer-between low high)))
;;          (let ((j (an-integer-between i high)))
;;            (let ((k (an-integer-between j high)))
;;              (require (= (+ (* i i) (* j j)) (* k k)))
;;              (list i j k)))))

(define (an-integer-between low high)
  (required (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (required (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(test-group "4.35"
  (test '(5 6 7 8 9 10) (all-of (an-integer-between 5 10)))
  (test '((3 4 5)) (all-of (a-pythagorean-triple-between 3 5))))

;; (define (solve-dwelling-puzzle)
;;   (let ((baker    (amb 1 2 3 4 5))
;;         (cooper   (amb 1 2 3 4 5))
;;         (fletcher (amb 1 2 3 4 5))
;;         (miller   (amb 1 2 3 4 5))
;;         (smith    (amb 1 2 3 4 5)))
;;
;;     (required (distinct? (list baker cooper fletcher miller smith)))
;;     (required (not (= baker 5)))
;;     (required (not (= cooper 1)))
;;     (required (not (= fletcher 5)))
;;     (required (not (= fletcher 1)))
;;     (required (> miller cooper))
;;     (required (not (= (abs (- smith fletcher)) 1)))
;;     (required (not (= (abs (- fletcher cooper)) 1)))
;;
;;     (list (list 'baker    baker)
;;           (list 'cooper   cooper)
;;           (list 'fletcher fletcher)
;;           (list 'miller   miller)
;;           (list 'smith    smith))))
;;
;; (solve-dwelling-puzzle)
