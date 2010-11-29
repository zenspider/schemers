#lang r5rs

;;; Exercise 1.25:

;; Alyssa P. Hacker complains that we went to a lot of extra work in
;; writing `expmod'. After all, she says, since we already know how to
;; compute exponentials, we could have simply written

(define (fast-expt b n)
  (define (square n) (* n n))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; Is she correct? Would this procedure serve as well for our fast
;; prime tester? Explain.

