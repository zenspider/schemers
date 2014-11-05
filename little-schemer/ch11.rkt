#lang racket/base

(require "../sicp/lib/test.rkt")
(module+ test (require rackunit))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) preceding)
                    (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else
           (two-in-a-row-b? (car lat) (cdr lat))))))

(test-group "two-in-a-row?"
 (test #f (two-in-a-row? '(a b c d)))
 (test #f (two-in-a-row? '(a b a d)))
 (test #t (two-in-a-row? '(a b b d)))
 (test #t (two-in-a-row? '(a a c d)))
 (test #t (two-in-a-row? '(a b c c))))

(define sum-of-prefixes-b
  (lambda (sum tup)
    (cond ((null? tup) '())
          (else (cons (+ sum (car tup))
                 (sum-of-prefixes-b (+ sum (car tup)) (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(test-group "sum-of-prefixes"
  (test '(1 2 3 4 5)     (sum-of-prefixes '(1 1 1 1 1)))
  (test '(2 3 12 29 29)  (sum-of-prefixes '(2 1 9 17 0))))

;; 11th commandment: Use additional arguments when a function needs to
;;                   know what other arguments to the function have
;;                   been so far.

(define pick
  (lambda (n lat)
    (cond ((= 1 n) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond ((null? tup) '())
          (else
           (cons (pick (car tup) (cons (car tup) rev-pre))
                 (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(test-group "scramble"
  (test '(1 1 1 1 1 4 1 1 1 9) (scramble '(1 1 1 3 4 2 1 1 9 2)))
  (test '(1 1 1 1 1 1 1 1 1)   (scramble '(1 2 3 4 5 6 7 8 9))))
