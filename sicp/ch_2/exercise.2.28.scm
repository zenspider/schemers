#lang racket

(require "../lib/testes.rkt")

;;; Exercise 2.28.  

;; Write a procedure fringe that takes as argument a tree (represented
;; as a list) and returns a list whose elements are all the leaves of
;; the tree arranged in left-to-right order. For example,
;;
;; (define x (list (list 1 2) (list 3 4)))
;; 
;; (fringe x)
;; (1 2 3 4)
;;
;; (fringe (list x x))
;; (1 2 3 4 1 2 3 4)

(define (fringe l) 
  (if (null? l) null
      (append                           ; why append and not cons? :/
       (cond ((null? (car l)) null)
             ((list? (car l)) (fringe (car l)))
             (else (list (car l))))
       (cond ((null? (cdr l)) null)
             ((list? (cdr l)) (fringe (cdr l)))
             (else (list (cadr l)))))))

(assert-equal '()                (fringe '()))
(assert-equal '(1)               (fringe '(1)))
(assert-equal '(1 2)             (fringe '(1 2)))
(assert-equal '(1 2 3)           (fringe '((1) 2 3)))
(assert-equal '(1 2 3)           (fringe '(1 (2) 3)))
(assert-equal '(1 2 3)           (fringe '((1) (2) (3))))
(assert-equal '(1 2 3)           (fringe '(1 2 (3))))
(assert-equal '(1 2 3)           (fringe '(1 2 (3))))
(assert-equal '(1 2 3 4)         (fringe '((1 2) (3 4))))
(assert-equal '(1 2 3 4 1 2 3 4) (fringe '(((1 2) (3 4)) ((1 2) (3 4)))))
