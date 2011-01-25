
;;; Exercise 2.9:

;; The "width" of an interval is half of the difference between its
;; upper and lower bounds. The width is a measure of the uncertainty
;; of the number specified by the interval. For some arithmetic
;; operations the width of the result of combining two intervals is a
;; function only of the widths of the argument intervals, whereas for
;; others the width of the combination is not a function of the widths
;; of the argument intervals. Show that the width of the sum (or
;; difference) of two intervals is a function only of the widths of
;; the intervals being added (or subtracted).

(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define i3 (make-interval 1 2))
(define i4 (make-interval 4 8))

;; Show that the width of the sum of two intervals is a function of
;; the widths of the intervals being added.

;; um. duh? we're talking about addition here folks.
;;
;; (+ (a . b) (c . d)) => ((+ a c) . (+ b d))
;; (width ((+ a c) . (+ b d)))
;; => (/ (- (+ b d) (+ a c)) 2)
;; => (/ (+ (- b a) (- d c)) 2)
;; (width (a . b))
;; => (/ (- b a) 2)
;; (width (c . d))
;; => (/ (- c d) 2)
;; (+ (/ (- b a) 2) (/ (- c d) 2))
;; => (/ (+ (- b a) (- c d)) 2)

(interval-width i3)                     ; 1/2
(interval-width i4)                     ; 2
(interval-width (add-interval i3 i4))   ; 5/2 = 1/2 + 4/2
(interval-width (sub-interval i4 i3))   ; 3/2 = 4/2 - 1/2

;; Give examples to show that this is not true for multiplication or
;; division.

;; don't care
