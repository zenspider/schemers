#lang racket

(require "../lib/testes.rkt")

;;; Exercise 2.21:

;; The procedure `square-list' takes a list of numbers as argument and
;; returns a list of the squares of those numbers.
;; 
;;      (square-list (list 1 2 3 4))
;;      (1 4 9 16)
;; 
;; Here are two different definitions of `square-list'. Complete both
;; of them by filling in the missing expressions:
;; 
;;      (define (square-list items)
;;        (if (null? items)
;;            nil
;;            (cons <??> <??>)))
;; 
;;      (define (square-list items)
;;        (map <??> <??>))

(define (square n) (* n n))

(define (square-list1 items)
  (if (null? items) null
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

(assert-equal '(1 4 9 16) (square-list1 '(1 2 3 4)))
(assert-equal '(1 4 9 16) (square-list2 '(1 2 3 4)))
(done)
