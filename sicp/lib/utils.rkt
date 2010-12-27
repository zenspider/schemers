#lang racket

(provide accumulate
         accumulate-n
         enumerate-interval
         flatmap
         fold-left
         fold-right
         inc
         prime?
         square)

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) null
      (cons (accumulate   op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (enumerate-interval low high)
  (if (> low high) null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (inc n) (+ 1 n))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define accumulate fold-right)

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (divides? a b)
        (= (remainder b a) 0))
      (define (next n)
        (if (odd? n) (+ 2 n) (+ 1 n)))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
      (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (square n) (* n n))
