#lang racket/base

;;; Exercise 1.9:

;; *Exercise 1.9:* Each of the following two procedures defines a
;; method for adding two positive integers in terms of the procedures
;; `inc', which increments its argument by 1, and `dec', which
;; decrements its argument by 1.
;;
;;      (define (+ a b)
;;        (if (= a 0)
;;            b
;;            (inc (+ (dec a) b))))
;;
;;      (define (+ a b)
;;        (if (= a 0)
;;            b
;;            (+ (dec a) (inc b))))
;;
;; Using the substitution model, illustrate the process generated by
;; each procedure in evaluating `(+ 4 5)'. Are these processes
;; iterative or recursive?

;; shut the runtime up so my rake test finishes

(define (inc n) n)
(define (dec n) n)

;;; First Version:

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;;; Second Version:

(+ 4 5)
(+ (dec 4) (inc 5))                     ; => (+ 3 6)
(+ (dec 3) (inc 6))                     ; => (+ 2 7)
(+ (dec 2) (inc 7))                     ; => (+ 1 8)
(+ (dec 1) (inc 8))                     ; => (+ 0 9)
9
