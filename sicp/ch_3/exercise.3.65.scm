#lang racket/base

(require "../lib/streams.scm")
(require "../lib/test.rkt")

;;; Exercise 3.65

;; Use the series
;;
;;                  1     1     1
;;      ln 2 = 1 - --- + --- - --- + ...
;;                  2     3     4
;;
;; to compute three sequences of approximations to the natural
;; logarithm of 2, in the same way we did above for [pi].  How
;; rapidly do these sequences converge?

(define (partial-sums s)
  (stream-cons (stream-car s)
               (stream-add (partial-sums s)
                           (stream-cdr s))))

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (stream-scale (partial-sums (pi-summands 1)) 4))

(define (log2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (log2-summands (+ n 1)))))

(define log2-stream
  (partial-sums (log2-summands 1)))

(define (euler-transform s)
  (define (square x) (* x x))
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(stream-head log2-stream 10)
(stream-head (euler-transform log2-stream) 10)
(stream-head (accelerated-sequence euler-transform log2-stream) 9)
;; 10 causes division by zero because of floating point equality :/
