#lang racket

;;; Exercise 1.22:

;; Most Lisp implementations include a primitive called `runtime' that
;; returns an integer that specifies the amount of time the system has
;; been running (measured, for example, in microseconds). The
;; following `timed-prime-test' procedure, when called with an integer
;; n, prints n and checks to see if n is prime. If n is prime, the
;; procedure prints three asterisks followed by the amount of time
;; used in performing the test.

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (divides? a b)
        (= (remainder b a) 0))
      (define (square n) (* n n))
      (define (next n) (+ test-divisor 1))
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

;; Using this procedure, write a procedure `search-for-primes' that
;; checks the primality of consecutive odd integers in a specified
;; range. Use your procedure to find the three smallest primes larger
;; than 1000; larger than 10,000; larger than 100,000; larger than
;; 1,000,000. Note the time needed to test each prime. Since the
;; testing algorithm has order of growth of [theta](_[sqrt]_(n)), you
;; should expect that testing for primes around 10,000 should take
;; about _[sqrt]_(10) times as long as testing for primes around 1000.
;; Do your timing data bear this out? How well do the data for 100,000
;; and 1,000,000 support the _[sqrt]_(n) prediction? Is your result
;; compatible with the notion that programs on your machine run in
;; time proportional to the number of steps required for the
;; computation?

(define (!= a b) (not (= a b)))
(define (search-for-primes from count)
  (and (!= count 0)
       (if (timed-prime-test from)
           (search-for-primes (+ from 1) (- count 1))
           (search-for-primes (+ from 1) count))))

(search-for-primes 1001 3)

;; 1009    = 0.0009765625 ms
;; 1013    = 0.0009765625 ms
;; 1019    = 0.0009765625 ms

(search-for-primes 10001 3)

;; 10007   = 0.001953125 ms
;; 10009   = 0.001953125 ms
;; 10037   = 0.001953125 ms

(search-for-primes 100001 3)

;; 100003  = 0.0068359375 ms
;; 100019  = 0.007080078125 ms
;; 100043  = 0.005859375 ms

(search-for-primes 1000001 3)

;; 1000003 = 0.018798828125 ms
;; 1000033 = 0.018798828125 ms
;; 1000037 = 0.019775390625 ms

(sqrt 10)                               ; 3.162

;; seems to fit the pattern quite well
