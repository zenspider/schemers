#!/usr/bin/env csi -s

(use test)
(require-library machine)
(import machine)
(require-library ec-eval)
(import ec-eval)

;;; Exercise 5.24

;; Implement `cond' as a new basic special form without reducing it to
;; `if'. You will have to construct a loop that tests the predicates
;; of successive `cond' clauses until you find one that is true, and
;; then use `ev-sequence' to evaluate the actions of the clause.

(define input-src-good
  '((exp (begin
           (define (append x y)
             (if (null? x)
                 y
                 (cons (car x) (append (cdr x) y))))

           (let ((l (append '(a b c) '(d e f))))
             (cond
              ((eq? (length l) 5) 'bad)
              ((eq? (length l) 6) 'good)
              (else 'else)))))))

(define input-src-bad
  '((exp (begin
           (define (append x y)
             (if (null? x)
                 y
                 (cons (car x) (append (cdr x) y))))

           (let ((l (append '(a b c) '(d))))
             (cond
              ((eq? (length l) 5) 'bad)
              ((eq? (length l) 6) 'good)))))))

(define input-src-else
  '((exp (begin
           (define (append x y)
             (if (null? x)
                 y
                 (cons (car x) (append (cdr x) y))))

           (let ((l (append '(a b c) '(d))))
             (cond
              ((eq? (length l) 5) 'bad)
              ((eq? (length l) 6) 'good)
              (else 'else)))))))

(assert-machine ec-eval input-src-good 'val 'good)
(assert-machine ec-eval input-src-bad  'val '*undefined*)
(assert-machine ec-eval input-src-else 'val 'else)
