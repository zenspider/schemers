(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; Exercise 2.48

;; A directed line segment in the plane can be represented as a pair
;; of vectors--the vector running from the origin to the start-point
;; of the segment, and the vector running from the origin to the
;; end-point of the segment. Use your vector representation from *Note
;; Exercise 2-46:: to define a representation for segments with a
;; constructor `make-segment' and selectors `start-segment' and
;; `end-segment'.

(define make-segment  cons)
(define start-segment car)
(define end-segment   cdr)

;; really? do we have to repeat the same shit over and over?
;; I'm not even gonna write tests for this...
