#!/usr/bin/env csi -s

(require rackunit)
(require-library machine)
(import machine)

;;; Exercise 5.21

;; Implement register machines for the following
;; procedures.  Assume that the list-structure memory operations are
;; available as machine primitives.
;;
;;   a. Recursive `count-leaves':

(define (count-leaves-r tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves-r (car tree))
                 (count-leaves-r (cdr tree))))))

;;   b. Recursive `count-leaves' with explicit counter:

(define (count-leaves-c tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x (cons 1 2))
(define y (list x x))

(test 4 (count-leaves-r y))
(test 4 (count-leaves-c y))

;; (define count-leaves-c-m
;;   (make-machine
;;    '(tree n t)
;;    (list xxx)
;;    '(controller
;;      ;; (define (count-leaves-c tree)
;;      ;;   (define (count-iter tree n)
;;      ;;     (cond ((null? tree) n)
;;      ;;           ((not (pair? tree)) (+ n 1))
;;      ;;           (else (count-iter (cdr tree)
;;      ;;                             (count-iter (car tree) n)))))
;;      ;;   (count-iter tree 0))
;;
;;      (assign n (const 0))
;;
;;      count-loop
;;
;;      (test (op null?) tree)
;;      (branch (label done))
;;
;;      (test (op pair?) tree)
;;      (branch (label recurse))
;;
;;      recurse
;;
;;      done
;; )))

;; (test 4 (count-leaves-r-m y))
;; (test 4 (count-leaves-c-m y))

;; (define factorial
;;   (make-machine
;;    '(n continue product)
;;    (list (list '= =) (list '- -) (list '* *))
;;    '(controller
;;        (assign continue (label fact-done))
;;      fact-loop
;;        (test (op =) (reg n) (const 1))
;;        (branch (label base-case))
;;        (save continue)
;;        (save n)
;;        (assign n (op -) (reg n) (const 1))
;;        (assign continue (label after-fact))
;;        (goto (label fact-loop))
;;      after-fact
;;        (restore n)
;;        (restore continue)
;;        (assign product (op *) (reg n) (reg product))
;;        (goto (reg continue))
;;      base-case
;;        (assign product (const 1))
;;        (goto (reg continue))
;;      fact-done)))
;;
;; (assert-machine factorial '((n 1)) 'product 1)
;; (assert-machine factorial '((n 2)) 'product 2)
;; (assert-machine factorial '((n 3)) 'product 6)
;; (assert-machine factorial '((n 4)) 'product 24)
;; (trace-register-on factorial 'product)
;; (assert-machine factorial '((n 5)) 'product 120)
;; (trace-register-off factorial 'product)
