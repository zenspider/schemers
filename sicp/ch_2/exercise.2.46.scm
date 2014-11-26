
(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; Exercise 2.46

;; A two-dimensional vector v running from the origin to a point can
;; be represented as a pair consisting of an x-coordinate and a
;; y-coordinate. Implement a data abstraction for vectors by giving a
;; constructor `make-vect' and corresponding selectors `xcor-vect' and
;; `ycor-vect'. In terms of your selectors and constructor, implement
;; procedures `add-vect', `sub-vect', and `scale-vect' that perform
;; the operations vector addition, vector subtraction, and multiplying
;; a vector by a scalar:
;;
;;      (x_1, y_1) + (x_2, y_2) = (x_1 + x_2, y_1 + y_2)
;;      (x_1, y_1) - (x_2, y_2) = (x_1 - x_2, y_1 - y_2)
;;                   s * (x, y) = (sx, sy)

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v x)
  (make-vect (* (xcor-vect v) x)
             (* (ycor-vect v) x)))

(let ((v1 (make-vect 3 4))
      (v2 (make-vect 2 3)))
  (assert-equal '(3 . 4) v1)
  (assert-equal 3 (xcor-vect v1))
  (assert-equal 4 (ycor-vect v1))
  (assert-equal '(5 . 7) (add-vect v1 v2))
  (assert-equal '(1 . 1) (sub-vect v1 v2))
  (assert-equal '(6 . 8) (scale-vect v1 2))
  (assert-equal '(3/2 . 2) (scale-vect v1 1/2)))

;; (assert-equal x y)
(done)
