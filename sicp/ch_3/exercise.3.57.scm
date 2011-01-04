#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.57

;; How many additions are performed when we compute
;; the nth Fibonacci number using the definition of `fibs' based on
;; the `add-streams' procedure?  Show that the number of additions
;; would be exponentially greater if we had implemented `(delay
;; <EXP>)' simply as `(lambda () <EXP>)', without using the
;; optimization provided by the `memo-proc' procedure described in
;; section *Note 3-5-1::.(5)

;; (assert-equal x y)
(done)
