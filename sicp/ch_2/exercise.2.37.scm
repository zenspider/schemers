#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; Exercise 2.37

;; Suppose we represent vectors v = (v_i) as sequences of numbers, and
;; matrices m = (m_(ij)) as sequences of vectors (the rows of the
;; matrix). For example, the matrix
;;
;;      +-         -+
;;      |  1 2 3 4  |
;;      |  4 5 6 6  |
;;      |  6 7 8 9  |
;;      +-         -+
;;
;; is represented as the sequence `((1 2 3 4) (4 5 6 6) (6 7 8 9))'.
;; With this representation, we can use sequence operations to
;; concisely express the basic matrix and vector operations. These
;; operations (which are described in any book on matrix algebra) are
;; the following:
;;
;;      (dot-product v w)      returns the sum ∑(i) v[i]*w[i]
;;
;;      (matrix-*-vector m v)  returns the vector t,
;;                             where t[i] = ∑j m[(ij)] v[j]
;;
;;      (matrix-*-matrix m n)  returns the matrix p,
;;                             where p[(ij)] = ∑k m[(ik)] n[(kj)]
;;
;;      (transpose m)          returns the matrix n,
;;                             where n[(ij)] = m[(ji)]
;;
;;    We can define the dot product as(4)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for
;; computing the other matrix operations. (The procedure
;; `accumulate-n' is defined in *Note Exercise 2-36::.)
;;
;;      (define (matrix-*-vector m v)
;;        (map <??> m))
;;
;;      (define (transpose mat)
;;        (accumulate-n <??> <??> mat))
;;
;;      (define (matrix-*-matrix m n)
;;        (let ((cols (transpose n)))
;;          (map <??> m)))

(define (identity-n . x) x)

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (apply map (cons identity-n mat)))    ; does this make me a bad person?

(define (transpose-slow mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(assert-equal 7 (dot-product '(1 2 3) '(-1 1 2)))

;; [ 2 -1 1 ]
;; [ 0 -2 1 ] * [1 2 3] = [3 -1 -3]
;; [ 1 -2 0 ]

(define v '(1 2 3))
(define m '((2 -1 1) (0 -2 1) (1 -2 0)))
(assert-equal '(3 -1 -3) (matrix-*-vector m v))

(assert-many (lambda (f)
               (assert-equal '((2 0 1) (-1 -2 -2) (1 1 0)) (f m)))
             transpose-slow
             transpose)

;; test data stolen from aja because I'm tired.
(assert-equal '((19 22) (43 50)) (matrix-*-matrix '((1 2) (3 4))
                                                  '((5 6) (7 8))))

(done)
