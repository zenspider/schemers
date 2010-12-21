#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;; Exercise 2.33

;; Fill in the missing expressions to complete the following
;; definitions of some basic list-manipulation operations as
;; accumulations:
;;
;;      (define (map p sequence)
;;        (accumulate (lambda (x y) <??>) null sequence))
;;
;;      (define (append seq1 seq2)
;;        (accumulate cons <??> <??>))
;;
;;      (define (length sequence)
;;        (accumulate <??> 0 sequence))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate inc 0 sequence))

(assert-equal '(1 4 9) (my-map square '(1 2 3)))
(assert-equal '(1 2 3 4 5 6) (my-append '(1 2 3) '(4 5 6)))
(assert-equal 0 (my-length '()))
(assert-equal 1 (my-length '(1)))
(assert-equal 2 (my-length '(1 2)))
(assert-equal 3 (my-length '(1 (2) 3)))

(done)
