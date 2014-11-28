#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.59:

;; Implement the `union-set' operation for the unordered-list
;; representation of sets.

(define (union-set s1 s2)
  (if (null? s2) s1
      (union-set (adjoin-set (car s2) s1) (cdr s2))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set
      (cons x set)))

(assert-equal '(1 2 3) (sort-< (union-set '(3) '(1 2))))
(done)
