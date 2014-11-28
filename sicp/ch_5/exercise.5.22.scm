#!/usr/bin/env csi -s

(require rackunit)
(require-library machine)
(import machine)
(require-library ec-eval)
(import ec-eval)
(use srfi-1)                            ; append!

;;; Exercise 5.22

;; *Note Exercise 3-12:: of section *Note 3-3-1:: presented an
;; `append' procedure that appends two lists to form a new list and an
;; `append!' procedure that splices two lists together. Design a
;; register machine to implement each of these procedures. Assume that
;; the list-structure memory operations are available as primitive
;; operations.

(define (identity x) x)

(define x '(1 2 3))
(define y '(4 5 6))
(test '(1 2 3 4 5 6) (append x y))
(test '(1 2 3) (identity x))
(test '(4 5 6) (identity y))

(define x '(1 2 3))
(define y '(4 5 6))
(test '(1 2 3 4 5 6) (append! x y))
(test '(1 2 3 4 5 6) (identity x))
(test '(4 5 6) (identity y))

;; TODO: such a mental block it isn't funny...
;;
;; (define x '(1 2 3))
;; (define y '(4 5 6))
;; (test '(1 2 3 4 5 6) (my-append x y))
;; (test '(1 2 3) (identity x))
;; (test '(4 5 6) (identity y))

(define (my-append! l1 l2)
  (if (null? (cdr l1))
      (set-cdr! l1 l2)
      (my-append! (cdr l1) l2))
  l1)

(define (my-append! l1 l2)
  (define (iterate l)
    (if (null? (cdr l))
        (set-cdr! l l2)
        (iterate (cdr l))))
  (iterate l1)
  l1)

(define x '(1 2 3))
(define y '(4 5 6))
(test '(1 2 3 4 5 6) (my-append! x y))
(test '(1 2 3 4 5 6) (identity x))
(test '(4 5 6) (identity y))

(define machine-append!
  (make-machine
   '(tree l1 l2 l t) ; result is in l1
   (list (list 'cdr cdr) (list 'set-cdr! set-cdr!) (list 'null? null?))
   '(controller
       (assign l (reg l1))
     iter-loop
       (assign t (op cdr) (reg l))
       (test (op null?) (reg t))
       (branch (label done))
       ;; not done, iterate on cdr
       (assign l (op cdr) (reg l))
       (goto (label iter-loop))
     done
       (perform (op set-cdr!) (reg l) (reg l2)))))

(define x '(1 2 3))
(define y '(4 5 6))
(assert-machine machine-append!
                (list (list 'l1 x) (list 'l2 y))
                'l1 '(1 2 3 4 5 6))
(test '(1 2 3 4 5 6) (identity x))
(test '(4 5 6) (identity y))

