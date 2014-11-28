#lang racket/base

;;; Exercise 1.32:

;;   a. Show that `sum' and `product' (*Note Exercise 1-31::) are both
;;      special cases of a still more general notion called
;;      `accumulate' that combines a collection of terms, using some
;;      general accumulation function:
;;
;;           (accumulate combiner null-value term a next b)
;;
;;      `Accumulate' takes as arguments the same term and range
;;      specifications as `sum' and `product', together with a
;;      `combiner' procedure (of two arguments) that specifies how the
;;      current term is to be combined with the accumulation of the
;;      preceding terms and a `null-value' that specifies what base
;;      value to use when the terms run out. Write `accumulate' and
;;      show how `sum' and `product' can both be defined as simple
;;      calls to `accumulate'.

(define (prod-old f a b n)
  (if (> a b) 1
      (* (f a)
         (prod-old f (n a) b n))))

(define (accumulate f-c nullv f a b n)
  (if (> a b) nullv
      (f-c (f a)
         (accumulate f-c nullv f (n a) b n))))

(define (inc n) (+ n 1))
(define (identity n) n)

(= (* 10 9 8 7 6 5 4 3 2 1)
   (prod-old identity 1 10 inc))        ; #t

(= (* 10 9 8 7 6 5 4 3 2 1)
   (accumulate * 1 identity 1 10 inc))  ; #t

(define (prod-new f a b n)
  (accumulate * 1 f a b n))

(= (* 10 9 8 7 6 5 4 3 2 1)
   (prod-new identity 1 10 inc))        ; #t

;;   b. If your `accumulate' procedure generates a recursive process,
;;      write one that generates an iterative process. If it generates
;;      an iterative process, write one that generates a recursive
;;      process.

;; god this is getting old

(define (accumulate-i f-c nullv f a b n)
  (define (iterate a result)
    (if (> a b) result
        (iterate (n a) (f-c (f a) result))))
  (iterate a nullv))

(= (* 10 9 8 7 6 5 4 3 2 1)
   (accumulate-i * 1 identity 1 10 inc))    ; #t
