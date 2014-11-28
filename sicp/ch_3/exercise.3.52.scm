#lang racket/base

(require "../lib/streams.scm")
(require rackunit)

;;; Exercise 3.52

;; Consider the sequence of expressions
;;
;;      (define sum 0)
;;
;;      (define (accum x)
;;        (set! sum (+ x sum))
;;        sum)
;;
;;      (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;;      (define y (stream-filter even? seq))
;;      (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                               seq))
;;
;;      (stream-ref y 7)
;;
;;      (display-stream z)
;;
;; What is the value of `sum' after each of the above expressions is
;; evaluated?  What is the printed response to evaluating the
;; `stream-ref' and `display-stream' expressions?  Would these
;; responses differ if we had implemented `(delay <EXP>)' simply as
;; `(lambda () <EXP>)' without using the optimization provided by
;; `memo-proc'?  Explain


(define sum 0)

(check-equal? 0 sum "initial value")

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

;; seq = 1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210
;;   y = 6 10 28 36 66 78 120 136 ...
;;   z = 10 15 45 55 ...

(stream-ref y 7)

(check-equal? 136 sum "(stream-ref y 7)")       ; the 7th even number in seq

(stream-display z)

;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210

(check-equal? 210 sum "(stream-display z)")     ; the last item in sum
