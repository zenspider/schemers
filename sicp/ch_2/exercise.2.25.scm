#lang racket

(require "../lib/testes.rkt")

;; Exercise 2.25.

;; Give combinations of cars and cdrs that will pick 7 from each of
;; the following lists:
;;
;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))

(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

(assert-equal 7 (car (cdaddr a)))
(assert-equal 7 (caar b))
(assert-equal 7 (cadr (cadr (cadr (cadr (cadr (cadr c)))))))
;; or
(assert-equal 7 (cadadr (cadadr (cadadr c))))
(done)
