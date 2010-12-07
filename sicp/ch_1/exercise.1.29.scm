#lang racket

;;; Exercise 1.29:

;; Simpson's Rule is a more accurate method of numerical integration
;; than the method illustrated above. Using Simpson's Rule, the
;; integral of a function f between a and b is approximated as
;; 
;;      h
;;      -(y[0] + 4y[1] + 2y[2] + 4y[3] + 2y[4] + ... + 2y[n-2] + 4y[n-1] + y[n]
;;      3
;; 
;; where h = (b - a)/n, for some even integer n, and y_k = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f, a, b, and n and returns the
;; value of the integral, computed using Simpson's Rule. Use your
;; procedure to integrate `cube' between 0 and 1 (with n = 100 and n =
;; 1000), and compare the results to those of the `integral' procedure
;; shown above.

(define (sum f a b n)
  (if (> a b) 0
      (+ (f a)
         (sum f (n a) b n))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) b add-dx) dx))

(define (inc n) (+ n 1))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (* (f (+ a (* h k)))
                   (cond ((or (= 0 k) (= n k)) 1)
                         ((odd? k) 4)
                         (else 2))))

  (* (/ h 3) (sum y 0 n inc)))

(define (cube n) (* n n n))

(integral cube 0 1 0.01)                ; 0.24998750000000042
(integral cube 0 1 0.001)               ; 0.249999875000001

(simpson cube 0 1 100)                  ; 1/4
(simpson cube 0 1 1000)                 ; 1/4
