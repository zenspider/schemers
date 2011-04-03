#!/usr/bin/env csi -s

(use test)
(use numbers)
(require-library lazy-eval)
(import lazy-eval)

;;; Exercise 4.32

;; Give some examples that illustrate the difference between the
;; streams of *Note Chapter 3:: and the "lazier" lazy lists described
;; in this section. How can you take advantage of this extra laziness?

(define env (setup-environment))

(define-syntax eval-define
  (syntax-rules ()
    ((_ ast) (eval (quote ast) env))))

(define-syntax test-eval
  (syntax-rules ()
    ((_ exp ast) (test exp (eval (quote ast) env)))))

(define-syntax test-real
  (syntax-rules ()
    ((_ exp ast) (test exp (actual-value (quote ast) env)))))

(eval-define (define (cons x y) (lambda (m) (m x y))))
(eval-define (define (car z) (z (lambda (p q) p))))
(eval-define (define (cdr z) (z (lambda (p q) q))))
(eval-define (define (list-ref items n)
               (if (= n 0)
                   (car items)
                   (list-ref (cdr items) (- n 1)))))
(eval-define (define (map proc items)
               (if (null? items) '()
                   (cons (proc (car items))
                         (map proc (cdr items))))))
(eval-define (define (scale-list items n)
               (map (lambda (x) (* x n)) items)))
(eval-define (define (add-lists l1 l2)
               (cond ((null? l1) l2)
                     ((null? l2) l1)
                     (else (cons (+ (car l1) (car l2))
                                 (add-lists (cdr l1) (cdr l2)))))))

(eval-define (define ones     (cons 1 ones)))
(eval-define (define integers (cons 1 (add-lists ones integers))))

(test-real 18 (list-ref integers 17))
(test-real 36 (list-ref (map (lambda (x) (* x 2)) integers) 17))
(test-real 36 (list-ref (scale-list integers 2) 17))

(eval-define (define (integral integrand initial-value dt)
               (define int
                 (cons initial-value
                       (add-lists (scale-list integrand dt) int)))
               int))

(test-real 1.055 (list-ref (integral integers 1 0.001) 10))

(eval-define (define (solve f y0 dt)
               (define y  (integral dy y0 dt))
               (define dy (map f y))
               y))

(test-real 1.10512 (list-ref (solve (lambda (x) x) 1 0.001) 100))
