#lang racket

(require "../lib/testes.rkt")

;; Exercise 2.32.

;; We can represent a set as a list of distinct elements, and we can
;; represent the set of all subsets of the set as a list of lists. For
;; example, if the set is (1 2 3), then the set of all subsets is
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
;; definition of a procedure that generates the set of subsets of a
;; set and give a clear explanation of why it works:
;;
;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))

(define (subsets l)
  (if (null? l) (list l)
      (let ((rest (subsets (cdr l)))
            (append-car (lambda (x) (cons (car l) x))))
        (append rest (map append-car rest)))))

(assert-equal '(()) (subsets '()))

(assert-equal '(() (1))
              (subsets '(1)))

(assert-equal '(() (2) (1) (1 2))
              (subsets '(1 2)))

(assert-equal '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
              (subsets '(1 2 3)))
