#lang racket/base

(require "../lib/streams.scm")
(require "../lib/test.rkt")

;;; Exercise 3.67

;; Modify the `pairs' procedure so that `(pairs integers integers)'
;; will produce the stream of _all_ pairs of integers (i,j) (without
;; the condition i <= j). Hint: You will need to mix in an additional
;; stream.

(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-cons
     (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(define (sum-of-powers-of-2 n)
  (apply + (map (lambda (x) (expt 2 x)) (stream-head integers n))))

(test '(1 1) (stream-ref int-pairs (sum-of-powers-of-2 (- 1 1))))
(test '(2 2) (stream-ref int-pairs (sum-of-powers-of-2 (- 2 1))))
(test '(3 3) (stream-ref int-pairs (sum-of-powers-of-2 (- 3 1))))
(test '(4 4) (stream-ref int-pairs (sum-of-powers-of-2 (- 4 1))))
(test '(5 5) (stream-ref int-pairs (sum-of-powers-of-2 (- 5 1))))

(stream-head int-pairs 200)
