
;;; Exercise 2.8:

;; Using reasoning analogous to Alyssa's, describe how the difference
;; of two intervals may be computed. Define a corresponding
;; subtraction procedure, called `sub-interval'.

(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; (add-interval (a . b) (c . d))             => ((+ a c) . (+ b d))
;; (sub-interval ((+ a c) . (+ b d)) (c . d)) => (a . b)

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define a (make-interval 1 2))
(define b (make-interval 4 8))

(sub-interval b a)                      ; (3 . 6)

(sub-interval (add-interval a b) a)     ; (4 . 8) aka b
(sub-interval (add-interval a b) b)     ; (1 . 2) aka a

;; I suspect I'm not getting something because this problem shouldn't
;; be this easy.
