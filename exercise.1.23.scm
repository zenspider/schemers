#lang racket

;;; Exercise 1.23:

;; The `smallest-divisor' procedure shown at the start of this section
;; does lots of needless testing: After it checks to see if the number
;; is divisible by 2 there is no point in checking to see if it is
;; divisible by any larger even numbers. This suggests that the values
;; used for `test-divisor' should not be 2, 3, 4, 5, 6, ..., but
;; rather 2, 3, 5, 7, 9, .... To implement this change, define a
;; procedure `next' that returns 3 if its input is equal to 2 and
;; otherwise returns its input plus 2. Modify the `smallest-divisor'
;; procedure to use `(next test-divisor)' instead of `(+ test-divisor
;; 1)'. With `timed-prime-test' incorporating this modified version of
;; `smallest-divisor', run the test for each of the 12 primes found in
;; *Note Exercise 1-22::. Since this modification halves the number of
;; test steps, you should expect it to run about twice as fast. Is
;; this expectation confirmed? If not, what is the observed ratio of
;; the speeds of the two algorithms, and how do you explain the fact
;; that it is different from 2?

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (divides? a b)
        (= (remainder b a) 0))
      (define (square n) (* n n))
      (define (next n)
        (if (odd? n) (+ 2 n) (+ 1 n)))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
      (find-divisor n 2))
  (= n (smallest-divisor n)))

(define runtime current-inexact-milliseconds)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (and (prime? n)
       (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " = ")
  (display elapsed-time)
  (display " ms")
  (newline))

(define (!= a b) (not (= a b)))

(define (search-for-primes from count)
  (and (!= count 0)
       (if (timed-prime-test from)
           (search-for-primes (+ from 1) (- count 1))
           (search-for-primes (+ from 1) count))))

(search-for-primes 1001 3)

;; old
;; 1009    = 0.0009765625 ms
;; 1013    = 0.0009765625 ms
;; 1019    = 0.0009765625 ms

;; new
;; 1009    = 0.0009765625 ms
;; 1013    = 0.0009765625 ms
;; 1019    = 0.0009765625 ms

;; about the same

(search-for-primes 10001 3)

;; old
;; 10007   = 0.001953125 ms
;; 10009   = 0.001953125 ms
;; 10037   = 0.001953125 ms

;; new
;; 10007   = 0.0009765625 ms
;; 10009   = 0.001953125 ms
;; 10037   = 0.0009765625 ms

;; chaotic, dunno.

(search-for-primes 100001 3)

;; old
;; 100003  = 0.0068359375 ms
;; 100019  = 0.007080078125 ms
;; 100043  = 0.005859375 ms

;; new
;; 100003  = 0.002197265625 ms
;; 100019  = 0.0029296875 ms
;; 100043  = 0.0029296875 ms

;; better than 2x faster... about 3x

(search-for-primes 1000001 3)

;; old
;; 1000003 = 0.018798828125 ms
;; 1000033 = 0.018798828125 ms
;; 1000037 = 0.019775390625 ms

;; new
;; 1000003 = 0.0078125 ms
;; 1000033 = 0.007080078125 ms
;; 1000037 = 0.008056640625 ms

;; about 2.5x faster