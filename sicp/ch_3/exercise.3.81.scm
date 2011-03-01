(use test)
(require-library streams)
(import streams)

;;; Exercise 3.81

;; *Note Exercise 3-6:: discussed generalizing the random-number
;; generator to allow one to reset the random-number sequence so as to
;; produce repeatable sequences of "random" numbers. Produce a stream
;; formulation of this same generator that operates on an input stream
;; of requests to `generate' a new random number or to `reset' the
;; sequence to a specified value and that produces the desired stream
;; of random numbers. Don't use assignment in your solution.

;; This assignment is stupid. I'd much rather make a REAL random stream.

(define (rand-update x) ;; primes 100006, 100007, and 100008
  (let ((a 1299817) (b 1299821) (m 1299827) (% modulo))
    (% (+ (* a x) b) m)))

(define (stream-rand seed)
  (define x (stream-cons seed
                         (stream-map rand-update x)))
  x)

(define rand (stream-rand 42))

(test 42                             (stream-ref rand 0))
(test (rand-update 42)               (stream-ref rand 1))
(test (rand-update (rand-update 42)) (stream-ref rand 2))

(define rand (stream-rand 42))

(test 42                             (stream-ref rand 0))
(test (rand-update 42)               (stream-ref rand 1))
