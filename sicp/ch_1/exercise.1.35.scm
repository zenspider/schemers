#lang racket/base

;;; Exercise 1.35:

;; Show that the golden ratio [phi] (section *Note 1-2-2::) is a fixed
;; point of the transformation x |-> 1 + 1/x,

;; we showed this umpteen times on previous homework. See 1.13.

;; and use this fact to compute [phi] by means of the `fixed-point'
;; procedure.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(< (- (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
      (/ (+ 1 (sqrt 5)) 2))
   tolerance)
