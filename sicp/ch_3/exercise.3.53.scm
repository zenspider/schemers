#lang racket/base

(require "../lib/streams.scm")
(require "../lib/test.rkt")

;;; Exercise 3.53

;; Without running the program, describe the elements of the stream defined by:

(define s (stream-cons 1 (stream-add s s)))

;; s is a doubler: 1 2 4 8 16 ...

(test 1024 (stream-ref s 10))
