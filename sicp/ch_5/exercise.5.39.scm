#!/usr/bin/env csi -s

(use test)
(use srfi-1)

;;; Exercise 5.39

;; Write a procedure `lexical-address-lookup' that implements the new
;; lookup operation. It should take two arguments--a lexical address
;; and a run-time environment--and return the value of the variable
;; stored at the specified lexical address. `Lexical-address-lookup'
;; should signal an error if the value of the variable is the symbol
;; `*unassigned*'.(2) Also write a procedure `lexical-address-set!'
;; that implements the operation that changes the value of the
;; variable at a specified lexical address.

(define (lexical-address-lookup address env)
  (last (list-ref (list-ref env (first address)) (second address))))

'(lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) (* x y z))          ; <E1> x = (2, 0), y = (2, 1)
      (* a b x)                         ; <E2> x = (1, 0), y = (1, 0)
      (+ c d x))))

(let ((env-e1 '(((y 8) (z 9))
                ((a 3) (b 4) (c 5) (d 6) (e 7))
                ((x 1) (y 2)))))
  (test 1 (lexical-address-lookup '(2 0) env-e1))
  (test 2 (lexical-address-lookup '(2 1) env-e1))
  (test 5 (lexical-address-lookup '(1 2) env-e1))
  (test 9 (lexical-address-lookup '(0 1) env-e1)))

;;; Old Revisions:

;; (define (lexical-address-lookup address env)
;;   (let ((y (car  address))              ; up
;;         (x (cadr address)))             ; right
;;     (if (= y 0)
;;         (if (= x 0)
;;             (car env)                   ; broken
;;             (lexical-address-lookup (list 0 (- x 1)) (cdr env)))
;;         (lexical-address-lookup (list (- y 1) x) (cdr env)))))
;;
;; (define (lexical-address-lookup address env)
;;   (let ((y (car  address))              ; up
;;         (x (cadr address)))             ; right
;;     (if (= y 0)
;;         (cadr (list-ref (car env) x))
;;         (lexical-address-lookup (list (- y 1) x) (cdr env)))))
;;
;; (define (lexical-address-lookup address env)
;;   (cadr (list-ref (list-ref env (car address)) (cadr address))))
;;
;; (define (lexical-address-lookup address env)
;;   (cadar (drop (car (drop env (car address))) (cadr address))))
;;
;; (define (lexical-address-lookup address env)
;;   (last (first (drop (first (drop env (first address))) (second address)))))

