#!/usr/bin/env csi -s

(require rackunit)
(load "../lib/ch5-regsim.scm")

;;; Exercise 5.4

;; Specify register machines that implement each of
;; the following procedures.  For each machine, write a controller
;; instruction sequence and draw a diagram showing the data paths.
;;
;;   a. Recursive exponentiation:
;;
;;           (define (expt b n)
;;             (if (= n 0)
;;                 1
;;                 (* b (expt b (- n 1)))))
;;
;;   b. Iterative exponentiation:
;;
;;           (define (expt b n)
;;             (define (expt-iter counter product)
;;               (if (= counter 0)
;;                   product
;;                   (expt-iter (- counter 1) (* b product))))
;;             (expt-iter n 1))

(define expt-recursive
  (make-machine
   '(b n product continue)
   (list (list '= =)
         (list '* *)
         (list '- -))
   '(controller
       (assign continue (label expt-done))
     expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       ;; store and loop
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-expt))
       (goto (label expt-loop))
     after-expt
       ;; restore and unwind
       (restore n)
       (restore continue)
       (assign product (op *) (reg b) (reg product))
       (goto (reg continue))
     base-case
       ;; bottom out at 1
       (assign product (const 1))
       (goto (reg continue))
     expt-done)))

(define expt-iterative
  (make-machine
   '(b n counter product)
   (list (list '= =)
         (list '* *)
         (list '- -))
   '(controller
       (assign counter (reg n))
       (assign product (const 1))
     expt
       (test           (op =) (reg counter) (const 0))
       (branch (label expt-done))

       (assign counter (op -) (reg counter) (const 1))
       (assign product (op *) (reg product) (reg b))

       (goto (label expt))
     expt-done
     )))

(assert-machine expt-recursive '((b 2) (n 3)) 'product 8)
(assert-machine expt-iterative '((b 2) (n 3)) 'product 8)
