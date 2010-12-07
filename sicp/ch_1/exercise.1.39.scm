#lang racket

;;; Exercise 1.39:

;; A continued fraction representation of the tangent function was
;; published in 1770 by the German mathematician J.H. Lambert:
;; 
;;                    x
;;      tan x = ---------------
;;                      x^2
;;              1 - -----------
;;                        x^2
;;                  3 - -------
;;                      5 - ...
;; 
;; where x is in radians. Define a procedure `(tan-cf x k)' that
;; computes an approximation to the tangent function based on
;; Lambert's formula. `K' specifies the number of terms to compute, as
;; in *Note Exercise 1-37::.

;; aren't we done yet? wtf

(define (cont-frac n d i)
  (define (iterate i fraction)
    (if (< i 1) fraction
        (iterate (- i 1) 
                 (/ (n i)
                    (+ (d i) fraction)))))
  (iterate i 0))

(define (tan-cf x k)
  (let ((nx2 (- (* x x 1.0))))
    (cont-frac
     (lambda (i) (if (= i 1) x nx2))
     (lambda (i) (- (* 2 i) 1))
     k)))

(tan 10)                                ; 0.6483608274590867
(tan-cf 10 10)                          ; 0.2202426219807905
(tan-cf 10 15)                          ; 0.6479824862749116
(tan-cf 10 20)                          ; 0.6483608247520876
(tan-cf 10 25)                          ; 0.6483608274590856


