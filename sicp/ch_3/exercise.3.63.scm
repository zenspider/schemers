#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.63

;; Louis Reasoner asks why the `sqrt-stream'
;; procedure was not written in the following more straightforward
;; way, without the local variable `guesses':
;;
;;      (define (sqrt-stream x)
;;        (cons-stream 1.0
;;                     (stream-map (lambda (guess)
;;                                   (sqrt-improve guess x))
;;                                 (sqrt-stream x))))
;;
;; Alyssa P. Hacker replies that this version of the procedure is
;; considerably less efficient because it performs redundant
;; computation.  Explain Alyssa's answer.  Would the two versions
;; still differ in efficiency if our implementation of `delay' used
;; only `(lambda () <EXP>)' without using the optimization provided
;; by `memo-proc' (section *Note 3-5-1::)?

;; (assert-equal x y)
(done)
