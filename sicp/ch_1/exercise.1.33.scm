#lang racket/base

;;; Exercise 1.33:

;; You can obtain an even more general version of `accumulate'
;; (*Note Exercise 1-32) by introducing the notion of a "filter" on the
;; terms to be combined. That is, combine only those terms derived
;; from values in the range that satisfy a specified condition. The
;; resulting `filtered-accumulate' abstraction takes the same
;; arguments as accumulate, together with an additional predicate of
;; one argument that specifies the filter. Write `filtered-accumulate'
;; as a procedure.

(define (filtered-accumulate fold identity filter f a b n)
  (define (iterate a result)
    (if (> a b) result
        (iterate (n a) (fold (if (filter a) (f a) identity) result))))
  (iterate a identity))

;; Show how to express the following using `filtered-accumulate':
;;
;;   a. the sum of the squares of the prime numbers in the interval a
;;      to b (assuming that you have a `prime?' predicate already
;;      written)

(define (identity n) n)
(define (square n) (* n n))
(define (inc n) (+ 1 n))

(= (+ 1 3 5 7 9)
   (filtered-accumulate + 0 odd? identity 1 10 inc)) ; #t

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

(define (something a n)
  (filtered-accumulate + 0 prime? square a n inc))

(= (+ (square 11) (square 13) (square 17) (square 19))
   (something 10 20))                       ; #t

;;   b. the product of all the positive integers less than n that are
;;      relatively prime to n (i.e., all positive integers i < n such
;;      that GCD(i,n) = 1).

(define (poatpiltntarptn n)             ; wtf do you name this thing?!?
  (define (relatively-prime? a)
    (define (gcd a b)
      (if (= b 0) a (gcd b (remainder a b))))
    (= (gcd a n) 1))
  (filtered-accumulate * 1 relatively-prime? identity 2 (- n 1) inc))

(poatpiltntarptn 8)                     ; 105
