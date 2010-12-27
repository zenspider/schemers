#lang racket

;;; Exercise 2.7:

;; Alyssa's program is incomplete because she has not specified the
;; implementation of the interval abstraction. Here is a definition of
;; the interval constructor:

(define (make-interval a b) (cons a b))

(cdr (make-interval 1 2))

;; Define selectors `upper-bound' and `lower-bound' to complete the
;; implementation.
;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Electrical engineers will be using Alyssa's system to compute
;; electrical quantities. It is sometimes necessary for them to
;; compute the value of a parallel equivalent resistance R_p of two
;; resistors R_1 and R_2 using the formula
;;
;;                  1
;;      R_p = -------------
;;            1/R_1 + 1/R_2
;;
;; Resistance values are usually known only up to some tolerance
;; guaranteed by the manufacturer of the resistor. For example, if you
;; buy a resistor labeled "6.8 ohms with 10% tolerance" you can only
;; be sure that the resistor has a resistance between 6.8 - 0.68 =
;; 6.12 and 6.8 + 0.68 = 7.48 ohms. Thus, if you have a 6.8-ohm 10%
;; resistor in parallel with a 4.7-ohm 5% resistor, the resistance of
;; the combination can range from about 2.58 ohms (if the two
;; resistors are at the lower bounds) to about 2.97 ohms (if the two
;; resistors are at the upper bounds).

(define upper-bound cdr)
(define lower-bound car)

(/ 1 (+ (/ 1 6.12) (/ 1 (* 4.7 0.95)))) ; 2.58
(/ 1 (+ (/ 1 7.48) (/ 1 (* 4.7 1.05)))) ; 2.97

(define r1 (make-interval (* 0.90 6.8) (* 1.10 6.8)))
(define r2 (make-interval (* 0.95 4.7) (* 1.05 4.7)))

(/ 1 (+ (/ 1 (lower-bound r1)) (/ 1 (lower-bound r2)))) ; 2.58
(/ 1 (+ (/ 1 (upper-bound r1)) (/ 1 (upper-bound r2)))) ; 2.97
