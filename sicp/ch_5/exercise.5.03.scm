#!/usr/bin/env csi -s

(use test)
(load "../lib/ch5-regsim.scm")

;;; Exercise 5.3

;; Design a machine to compute square roots using Newton's method, as
;; described in section *Note 1-1-7:::
;;
;;      (define (sqrt x)
;;        (define (good-enough? guess)
;;          (< (abs (- (square guess) x)) 0.001))
;;        (define (improve guess)
;;          (average guess (/ x guess)))
;;        (define (sqrt-iter guess)
;;          (if (good-enough? guess)
;;              guess
;;              (sqrt-iter (improve guess))))
;;        (sqrt-iter 1.0))
;;
;; Begin by assuming that `good-enough?' and `improve' operations are
;; available as primitives. Then show how to expand these in terms of
;; arithmetic operations. Describe each version of the `sqrt' machine
;; design by drawing a data-path diagram and writing a controller
;; definition in the register-machine language.

(define (square n) (* n n))

(define (average a b) (/ (+ a b) 2))

(define (sqrt-orig x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess) guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(test 2.0 (sqrt-orig 4))

(define sqrt1
  (make-machine
   '(x guess t)
   (list (list 'good-enough?
               (lambda (guess x) (< (abs (- (square guess) x)) 0.001)))
         (list 'improve
               (lambda (guess x) (average guess (/ x guess)))))
   '(controller
       (assign guess (const 1.0))
     sqrt
       (test (op good-enough?) (reg guess) (reg x))
       (branch (label sqrt-done))
       (assign t (op improve) (reg guess) (reg x))
       (assign guess (reg t))
       (goto (label sqrt))
     sqrt-done)))

(define sqrt2
  (make-machine
   '(x guess improve t)
   (list (list '/ /)
         (list 'average average)
         (list 'good-enough?
               (lambda (guess x) (< (abs (- (square guess) x)) 0.001))))
   '(controller
       (assign guess (const 1.0))
     sqrt
       (test (op good-enough?) (reg guess) (reg x))
       (branch (label sqrt-done))

       ;; improve
       (assign t (op /) (reg x) (reg guess))
       (assign improve (op average) (reg guess) (reg t))

       (assign guess (reg improve))
       (goto (label sqrt))
     sqrt-done)))

(define sqrt3
  (make-machine
   '(x guess improve good-enough? guess2 diff t)
   (list (list '- -)
         (list '/ /)
         (list '< <)
         (list 'abs abs)
         (list 'average average)
         (list 'square square))
   '(controller
       (assign guess (const 1.0))
     sqrt

       ;; good-enough?
       (assign guess2       (op square) (reg guess))
       (assign diff         (op -)      (reg guess2) (reg x))
       (assign good-enough? (op abs)    (reg diff))
       (test                (op <)      (reg good-enough?) (const 0.001))
       (branch (label sqrt-done))

       ;; improve
       (assign t (op /) (reg x) (reg guess))
       (assign improve (op average) (reg guess) (reg t))

       (assign guess (reg improve))
       (goto (label sqrt))
     sqrt-done)))

(assert-machine sqrt1 '((x 4)) 'guess 2.0)
(assert-machine sqrt2 '((x 4)) 'guess 2.0)
(assert-machine sqrt3 '((x 4)) 'guess 2.0)
