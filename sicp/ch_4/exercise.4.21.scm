#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.21

;; Amazingly, Louis's intuition in *Note Exercise
;; 4-20:: is correct.  It is indeed possible to specify recursive
;; procedures without using `letrec' (or even `define'), although the
;; method for accomplishing this is much more subtle than Louis
;; imagined.  The following expression computes 10 factorial by
;; applying a recursive factorial procedure:(4)
;;
;;      ((lambda (n)
;;         ((lambda (fact)
;;            (fact fact n))
;;          (lambda (ft k)
;;            (if (= k 1)
;;                1
;;                (* k (ft ft (- k 1)))))))
;;       10)
;;
;;   a. Check (by evaluating the expression) that this really does
;;      compute factorials.  Devise an analogous expression for
;;      computing Fibonacci numbers.

;;     ;;; M-Eval input:
;;     ((lambda (n) ...)
;;
;;     ;;; M-Eval value:
;;     3628800
;;
;;     ;;; M-Eval input:
;;     (* 10 9 8 7 6 5 4 3 2 1)
;;
;;     ;;; M-Eval value:
;;     3628800

;; (holy shit!)

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 2)) (fib (- n 1)))))

(test 55 (fib 10))

(define (Y outer)                       ; derived via little schemer
  ((lambda (f) (f f))
   (lambda (f) (outer (lambda (x) ((f f) x))))))

(test 55 ((Y (lambda (fib)              ; clean happy version
               (lambda (n)
                 (if (< n 2) n
                     (+ (fib (- n 2)) (fib (- n 1))))))) 10))

(test 55 (((lambda (outer)              ; subst Y
             ((lambda (f) (f f))
              (lambda (f) (outer (lambda (x) ((f f) x))))))
           (lambda (fib)
             (lambda (n)
               (if (< n 2) n
                   (+ (fib (- n 2)) (fib (- n 1))))))) 10))

(test 55 ((lambda (n)                   ; apply outer - or just copy/mod fact
            ((lambda (f) (f f n))
             (lambda (f k)
               (if (< k 2) k
                   (+ (f f (- k 2)) (f f (- k 1)))))))
          10))

;;   b. Consider the following procedure, which includes mutually
;;      recursive internal definitions:

(define (f x)
  (define (even? n)
    (if (= n 0) true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0) false
        (even? (- n 1))))
  (even? x))

(test #f (f  1))
(test #t (f  2))
(test #f (f 21))
(test #t (f 22))

;;      Fill in the missing expressions to complete an alternative
;;      definition of `f', which uses neither internal definitions
;;      nor `letrec':
;;
;;           (define (f x)
;;             ((lambda (even? odd?)
;;                (even? even? odd? x))
;;              (lambda (ev? od? n)
;;                (if (= n 0) true (od? <??> <??> <??>)))
;;              (lambda (ev? od? n)
;;                (if (= n 0) false (ev? <??> <??> <??>)))))


(define (f2 x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false
         (ev? ev? od? (- n 1))))))

(test #f (f2  1))
(test #t (f2  2))
(test #f (f2 21))
(test #t (f2 22))
