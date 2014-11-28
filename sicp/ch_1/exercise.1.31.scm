#lang racket/base

;;; Exercise 1.31:

;;   a. The `sum' procedure is only the simplest of a vast number of
;;      similar abstractions that can be captured as higher-order
;;      procedures.(3) Write an analogous procedure called `product'
;;      that returns the product of the values of a function at points
;;      over a given range. Show how to define `factorial' in terms of
;;      `product'.

(define (prod f a b n)
  (if (> a b) 1
      (* (f a)
         (prod f (n a) b n))))

(define (inc n) (+ n 1))
(define (identity n) n)

(define (factorial n)
  (prod identity 1 n inc))

(= (* 10 9 8 7 6 5 4 3 2 1)
   (factorial 10))

;;  Also use `product' to compute approximations to [pi] using the
;;  formula(4)
;;
;;           pi   2 * 4 * 4 * 6 * 6 * 8 ...
;;           -- = -------------------------
;;            4   3 * 3 * 5 * 5 * 7 * 7 ...

;; the actual formula is pi/2 = 2 * 1-1/3 * 1+1/3 * 1-1/5 * 1+1/5 ...
;;                            = ∏ 4n^2 / 4n%2 - 1 for n from 0 .. infinity

(define (pi-wallace n)
  (define (term i)
    (/ (* 4 i i)
       (- (* 4 i i) 1)))
  (* 2.0 (prod term 1 n inc)))

(pi-wallace 100)                        ; 3.1337874906281624
(pi-wallace 1000)                       ; 3.1408077460303945
(pi-wallace 10000)                      ; 3.141514118681922

;;   b. If your `product' procedure generates a recursive process,
;;      write one that generates an iterative process. If it generates
;;      an iterative process, write one that generates a recursive
;;      process.

(define (prod-r f a b n)
  (define (iterate a p)
    (if (> a b) p
        (iterate (n a) (* p (f a)))))
  (iterate a 1))

(= (* 10 9 8 7 6 5 4 3 2 1)
   (prod-r identity 1 10 inc))          ; #t
