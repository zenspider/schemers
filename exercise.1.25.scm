#lang racket

;;; Exercise 1.25:

;; Alyssa P. Hacker complains that we went to a lot of extra work in
;; writing `expmod'. After all, she says, since we already know how to
;; compute exponentials, we could have simply written

(define (square n) (* n n))

(define (old-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else        (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fast-expt b n)
  (define (square n) (* n n))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (expmod base exp m)
  (remainder (expt base exp) m))

;; Is she correct? Would this procedure serve as well for our fast
;; prime tester? Explain.

(define (times-do n l)
  (cond ((= n 0) #f)
        (else
         (l)
         (times-do (- n 1) l))))

(define iters 100000)
(define max   100)

(define (rnd n) (+ 1 (random n)))       ; avoid divide by zero

(random-seed 0)
(time
 (times-do iters
           (lambda () (old-expmod (rnd max) (rnd max) (rnd max)))))

(random-seed 0)
(time
 (times-do iters
           (lambda () (new-expmod (rnd max) (rnd max) (rnd max)))))

(random-seed 0)
(time
 (times-do iters
           (lambda () (expmod (rnd max) (rnd max) (rnd max)))))

;; with 10,000 iters and 10,000 max
;; cpu time: 14311 real time: 14330 gc time: 128
;; cpu time: 22860 real time: 23011 gc time: 156
;; cpu time: 23062 real time: 23097 gc time: 158

;; with 100,000 iters and 100 max
;; cpu time: 406 real time: 406 gc time: 37
;; cpu time: 510 real time: 511 gc time: 4
;; cpu time: 417 real time: 419 gc time: 4

;; so... it appears to be faster only for smaller primes. For larger
;; primes, it seems to do a lot more work... oddly this seems to bear
;; out whether I use the built in expt or not.