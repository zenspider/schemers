#lang racket

;;; Exercise 2.5:

;; Show that we can represent pairs of nonnegative integers using only
;; numbers and arithmetic operations if we represent the pair a and b
;; as the integer that is the product 2^a 3^b. Give the corresponding
;; definitions of the procedures `cons', `car', and `cdr'.

(define (count-factors b n count)
  (if (> (remainder n b) 0) count
      (count-factors b (/ n b) (+ count 1))))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (count-factors 2 p 0))

(define (cdr p)
  (count-factors 3 p 0))

(car (cons 2 3))
(cdr (cons 2 3))
