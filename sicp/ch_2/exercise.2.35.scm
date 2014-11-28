#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; Exercise 2.35

;; Redefine `count-leaves' from section *Note 2-2-2:: as an
;; accumulation:
;;
;;      (define (count-leaves t)
;;        (accumulate <??> <??> (map <??> <??>)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (count-leaves2 t)
  (accumulate + 0 (map (lambda (l) (if (list? l) (count-leaves2 l) 1)) t)))

(define x (cons '(1 2) '(3 4)))
(assert-equal 3 (length x))
(assert-equal 2 (length (list x x)))

(assert-many (lambda (fut)
               (assert-equal 0 (fut '()))
               (assert-equal 1 (fut '(a)))
               (assert-equal 4 (fut x))
               (assert-equal 8 (fut (list x x))))
             count-leaves
             count-leaves2)

(assert-equal 0 0)
(done)
