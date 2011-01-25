
;;; Exercise 2.3:

;; Implement a representation for rectangles in a plane. (Hint: You
;; may want to make use of *Note Exercise 2-2::.) In terms of your
;; constructors and selectors, create procedures that compute the
;; perimeter and the area of a given rectangle. Now implement a
;; different representation for rectangles. Can you design your system
;; with suitable abstraction barriers, so that the same perimeter and
;; area procedures will work using either representation?

(define rect-width  'whatevs)
(define rect-length 'whatevs)

(define (rect-perimeter r)
  (+ (* 2 (rect-width r))
     (* 2 (rect-length r))))

(define (rect-area r)
  (* (rect-width r)
     (rect-length r)))

;; these will work no matter how you define rectangles (point w h) vs (p1 p2)
