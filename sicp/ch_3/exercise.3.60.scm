#lang racket/base

(require "../lib/streams.scm")
(require "../lib/test.rkt")

;;; Exercise 3.60

;; With power series represented as streams of
;; coefficients as in *Note Exercise 3-59::, adding series is
;; implemented by `add-streams'.  Complete the definition of the
;; following procedure for multiplying series:
;;
;;      (define (mul-series s1 s2)
;;        (stream-cons <??> (add-streams <??> <??>)))
;;
;; You can test your procedure by verifying that sin^2 x + cos^2 x =
;; 1, using the series from *Note Exercise 3-59::.

;; (load "ch_3/exercise.3.59.scm")

(define (stream-mul s1 s2)
  (stream-map * s1 s2))

(define nths (stream-map (lambda (x) (/ 1 x)) integers))
(define (integrate-series A) (stream-mul nths A))

(define cosine-series
  (stream-cons 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (stream-add (stream-scale (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define cos2-series      (mul-series cosine-series cosine-series))
(define sin2-series      (mul-series   sine-series   sine-series))
(define cos2+sin2-series (stream-add   sin2-series   cos2-series))

;; TODO: dunno how to test this yet
;; (stream-head cosine-series 10)
;; (stream-head sine-series 10)
;; (stream-head cos2-series 10)
;; (stream-head sin2-series 10)

(require compatibility/mlist)

(test-group "3.60"
            (test 1      (stream-ref  cos2+sin2-series 0))
            (test 0      (stream-ref  cos2+sin2-series 1))
            (test (stream-head cos2+sin2-series 2)
                  (mlist 1 0)))
