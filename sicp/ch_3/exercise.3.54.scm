#lang racket/base

(require (except-in "../lib/streams.scm" ones integers))
(require "../lib/test.rkt")

;;; Exercise 3.54

;; Define a procedure `mul-streams', analogous to
;; `add-streams', that produces the elementwise product of its two
;; input streams.  Use this together with the stream of `integers' to
;; complete the following definition of the stream whose nth element
;; (counting from 0) is n + 1 factorial:
;;
;;      (define factorials (stream-cons 1 (mul-streams <??> <??>)))

(define (stream-mul s1 s2)
  (stream-map * s1 s2))

(define ones     (stream-cons 1 ones))
(define integers (stream-cons 1 (stream-add ones integers)))

(define factorials
  (stream-cons 1 (stream-mul (stream-cdr integers) factorials)))

(test-group "3.54"
            (test '(1 2 6 24 120) (stream-head factorials 5))
            (test (* 1)         (stream-ref factorials 0))
            (test (* 1 2)       (stream-ref factorials 1))
            (test (* 1 2 3)     (stream-ref factorials 2))
            (test (* 1 2 3 4)   (stream-ref factorials 3))
            (test (* 1 2 3 4 5) (stream-ref factorials 4)))
