#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.5

;; "Monte Carlo integration" is a method of
;; estimating definite integrals by means of Monte Carlo simulation.
;; Consider computing the area of a region of space described by a
;; predicate P(x, y) that is true for points (x, y) in the region and
;; false for points not in the region.  For example, the region
;; contained within a circle of radius 3 centered at (5, 7) is
;; described by the predicate that tests whether (x - 5)^2 + (y -
;; 7)^2 <= 3^2.  To estimate the area of the region described by such
;; a predicate, begin by choosing a rectangle that contains the
;; region.  For example, a rectangle with diagonally opposite corners
;; at (2, 4) and (8, 10) contains the circle above.  The desired
;; integral is the area of that portion of the rectangle that lies in
;; the region.  We can estimate the integral by picking, at random,
;; points (x,y) that lie in the rectangle, and testing P(x, y) for
;; each point to determine whether the point lies in the region.  If
;; we try this with many points, then the fraction of points that
;; fall in the region should give an estimate of the proportion of
;; the rectangle that lies in the region.  Hence, multiplying this
;; fraction by the area of the entire rectangle should produce an
;; estimate of the integral.
;;
;; Implement Monte Carlo integration as a procedure
;; `estimate-integral' that takes as arguments a predicate `P', upper
;; and lower bounds `x1', `x2', `y1', and `y2' for the rectangle, and
;; the number of trials to perform in order to produce the estimate.
;; Your procedure should use the same `monte-carlo' procedure that
;; was used above to estimate [pi].  Use your `estimate-integral' to
;; produce an estimate of [pi] by measuring the area of a unit circle.
;;
;; You will find it useful to have a procedure that returns a number
;; chosen at random from a given range.  The following
;; `random-in-range' procedure implements this in terms of the
;; `random' procedure used in section *Note 1-2-6::, which returns a
;; nonnegative number less than its input.(3)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral test x1 y1 x2 y2 trials)
  ;;  % = pi*rr / (w * h)
  ;; pi = % * w * h / rr
  (let ((h (- y2 y1))
        (w (- x2 x1))
        (% (monte-carlo trials (lambda () (test (random-in-range x1 x2)
                                                (random-in-range y1 y2))))))
    (/ (* % w h) (square (/ w 2)))))

(let ((w 10000)
      (h 10000)
      (i 100000))
  (let ((midx (/ w 2))
        (midy (/ h 2))
        (rr   (square (/ w 2))))

    (define (in-circle x y)
      (<= (+ (square (- x midx)) (square (- y midy))) rr))

    (assert-float-d pi (* 1.0 (estimate-integral in-circle 0 0 w h i)) 0.01)))
(done)
