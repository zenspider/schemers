#lang racket

;;; Exercise 2.11:

;; In passing, Ben also cryptically comments: "By testing the signs of
;; the endpoints of the intervals, it is possible to break
;; `mul-interval' into nine cases, only one of which requires more
;; than two multiplications." Rewrite this procedure using Ben's
;; suggestion.

(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

;; there are many things I don't like about this. Let's clean it up first

(define (mul-interval-1 x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; this is a lot cleaner in my mind:

(define (mul-interval-2 x y)
  (let ((lx (lower-bound x)) (ux (upper-bound x))
        (ly (lower-bound y)) (uy (upper-bound y)))
    (let ((lxly (* lx ly))
          (lxuy (* lx uy))
          (uxly (* ux ly))
          (uxuy (* ux uy)))
      (make-interval (min lxly lxuy uxly uxuy)
                     (max lxly lxuy uxly uxuy)))))

;;   lx ux
;;   | X |
;;   ly uy
;;
;; lxly lxuy
;; uxly uxuy

;; upper bounds < lower bounds = invalid
;; lx+ ux+ ly+ uy-
;; lx+ ux- ly+ uy+
;; lx+ ux- ly+ uy-
;; lx+ ux- ly- uy+
;; lx+ ux- ly- uy-
;; lx- ux+ ly+ uy-
;; lx- ux- ly+ uy-

;; lx+ ux+ ly+ uy+ = lxly+      uxuy+
;; lx+ ux+ ly- uy+ = uxly-      uxuy+
;; lx+ ux+ ly- uy- = uxly-      lxuy-
;; lx- ux+ ly+ uy+ = lxuy-      uxuy+
;; lx- ux+ ly- uy+ = lxuy/lyux- lxly/uxuy+ moved to bottom
;; lx- ux+ ly- uy- = uxuy-      lxuy+
;; lx- ux- ly+ uy+ = lxuy-      uxly-
;; lx- ux- ly- uy+ = lxuy-      uxly+
;; lx- ux- ly- uy- = uxuy+      lxly+

(define (mul-interval x y)
  (let ((lx (lower-bound x)) (ux (upper-bound x))
        (ly (lower-bound y)) (uy (upper-bound y)))
    (let ((lx- (< lx 0)) (lx+ (>= lx 0))
          (ly- (< ly 0)) (ly+ (>= ly 0))
          (ux- (< ux 0)) (ux+ (>= ux 0))
          (uy- (< uy 0)) (uy+ (>= uy 0)))
      (cond ((and lx+ ux+ ly+ uy+) (make-interval (* lx ly) (* ux uy)))
            ((and lx+ ux+ ly- uy+) (make-interval (* ux ly) (* ux uy)))
            ((and lx+ ux+ ly- uy-) (make-interval (* ux ly) (* lx uy)))
            ((and lx- ux+ ly+ uy+) (make-interval (* lx uy) (* ux uy)))
            ((and lx- ux+ ly- uy-) (make-interval (* ux ly) (* lx ly)))
            ((and lx- ux- ly+ uy+) (make-interval (* lx uy) (* ux ly)))
            ((and lx- ux- ly- uy+) (make-interval (* lx uy) (* lx ly)))
            ((and lx- ux- ly- uy-) (make-interval (* ux uy) (* lx ly)))
            (else (make-interval (min (* lx uy) (* ux ly))
                                 (max (* lx ly) (* ux uy))))))))

(define (i= i j)
  (and (= (upper-bound i) (upper-bound j))
       (= (lower-bound i) (lower-bound j))))

(define (test a b c d)
  (let ((i (make-interval a b))
        (j (make-interval c d)))
    (i= (mul-interval-1 i j) (mul-interval i j))))

(test +1 +2 +4 +8)
(test +1 +2 -4 +8)
(test +1 +2 -8 -4)
(test -1 +2 +4 +8)
(test -1 +2 -4 +8) ;; edge case
(test -1 +2 -8 -4)
(test -2 -1 +4 +8)
(test -2 -1 -4 +8)
(test -2 -1 -8 -4)
