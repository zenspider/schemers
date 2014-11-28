#lang racket/base

;;; Exercise 1.12:

;; The following pattern of numbers is called "Pascal's triangle".
;;
;;              1
;;            1   1
;;          1   2   1
;;        1   3   3   1
;;      1   4   6   4   1
;;
;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it.(4)
;; Write a procedure that computes elements of Pascal's triangle by
;; means of a recursive process.

(define (pt r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pt (- r 1) (- c 1))
         (pt (- r 1) c))))

(pt 6 2)                                ; 5
(pt 6 3)                                ; 10
(pt 7 1)                                ; 1
(pt 7 2)                                ; 6
(pt 7 3)                                ; 15
(pt 7 4)                                ; 20
(pt 7 5)                                ; 15
(pt 7 6)                                ; 6
(pt 7 7)                                ; 1

;; time = 25 min
