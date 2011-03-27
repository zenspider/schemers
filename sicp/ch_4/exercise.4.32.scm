#!/usr/bin/env csi -s

(use test)
(require-library lazy-eval)
(import lazy-eval)

;;; Exercise 4.32

;; Give some examples that illustrate the difference between the
;; streams of *Note Chapter 3:: and the "lazier" lazy lists described
;; in this section. How can you take advantage of this extra laziness?

(define env (setup-environment))

(define-syntax test-eval
  (syntax-rules ()
    ((_ exp ast) (test exp (eval ast env)))))

(test-eval 'ok '(define (cons x y) (lambda (m) (m x y))))
(test-eval 'ok '(define (car z) (z (lambda (p q) p))))
(test-eval 'ok '(define (cdr z) (z (lambda (p q) q))))
(test-eval 'ok '(define (list-ref items n)
                  (if (= n 0)
                      (car items)
                      (list-ref (cdr items) (- n 1)))))
(test-eval 'ok '(define (map proc items)
                  (if (null? items) '()
                      (cons (proc (car items))
                            (map proc (cdr items))))))
(test-eval 'ok '(define (scale-list items n)
                  (map (lambda (x) (* x n)) items)))
(test-eval 'ok '(define (add-lists l1 l2)
                  (cond ((null? l1) l2)
                        ((null? l2) l1)
                        (else (cons (+ (car l1) (car l2))
                                    (add-lists (cdr l1) (cdr l2)))))))

(test-eval 'ok '(define ones     (cons 1 ones)))
(test-eval 'ok '(define integers (cons 1 (add-lists ones integers))))

(test 18 (actual-value '(list-ref integers 17) env))
(test 36 (actual-value '(list-ref (map (lambda (x) (* x 2)) integers) 17) env))
(test 36 (actual-value '(list-ref (scale-list integers 2) 17) env))

(test-eval 'ok '(define (integral integrand initial-value dt)
                  (define int
                    (cons initial-value
                          (add-lists (scale-list integrand dt) int)))
                  int))

(test 1.055 (actual-value '(list-ref (integral integers 1 0.001) 10) env))

(test-eval 'ok '(define (solve f y0 dt)
                  (define y  (integral dy y0 dt))
                  (define dy (map f y))
                  y))

(test-eval 2.716924 '(list-ref (solve (lambda (x) x) 1 0.001) 10))