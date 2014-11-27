#lang racket/base

(provide eqan? ** pick div)

(require "lib/shared.rkt")

;;; Chapter 4
;; pg 59

(test 68 (add1 67))
(test 68 (sub1 69))
(test #f (zero? 42))
(test #t (zero? 0))

(define ++
  (lambda (m n)
    (cond ((zero? n) m)
          (else (+ (add1 m) (sub1 n))))))

(test 7 (++ 3 4))

;; pg 61

(define --
  (lambda (m n)
    (cond ((zero? n) m)
          (else (-- (sub1 m) (sub1 n))))))

(test 4 (-- 7 3))

(define tup?
  (lambda (l)
    (cond ((null? l) #t)
          ((number? (car l)) (tup? (cdr l)))
          (else #f))))

(test #t (tup? '(1 2 3)))
(test #f (tup? '(1 b 3)))
(test #f (tup? '(1 (2 3) 4)))

(define addtup
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ (car l) (addtup (cdr l)))))))

(test 0 (addtup '()))
(test 1 (addtup '(1)))
(test 6 (addtup '(1 2 3)))

(define **
  (lambda (m n)
    (cond
     ;; ((< n m) (** n m))
     ((zero? m) 0)
     ;; ((zero? n) 0)
     ;; ((equal? 1 m) n)
     ;; ((equal? 1 n) m)
     (else (+ n (** n (sub1 m)))))))

(test 0 (** 5 0))
(test 0 (** 0 5))
(test 5 (** 1 5))
(test 5 (** 5 1))
(test 6 (** 2 3))
(test 6 (** 3 2))

(define tup+
  (lambda (t1 t2)
    (cond ((and (null? t1) (null? t2)) '())
          ((null? t1) t2)
          ((null? t2) t1)
          (else (cons (+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

(test '(11 11 11 11 11) (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))
(test '(2 4 6 4 5) (tup+ '(1 2 3) '(1 2 3 4 5)))
(test '(2 4 6 4 5) (tup+ '(1 2 3 4 5) '(1 2 3)))

;; pg 73
(define >>
  (lambda (n m)
    (cond ((zero? n) #f)
          ((zero? m) #t)
          (else (>> (sub1 n) (sub1 m))))))

(define <<
  (lambda (n m)
    (cond ((zero? m) #f)
          ((zero? n) #t)
          (else (<< (sub1 n) (sub1 m))))))

(define ==
  (lambda (n m)
    (not (or (<< n m) (>> n m)))))

(test #t (== 3 3))
(test #f (== 1 2))
(test #f (== 2 1))

;; pg 74

(define ^^
  (lambda (n exp)
    (cond ((zero? exp) 1)
          ((== 1 exp) n)
          (else (** n (^^ n (sub1 exp)))))))

(test 1 (^^ 1 1))
(test 8 (^^ 2 3))
(test 125 (^^ 5 3))

(define div
  (lambda (n m)
    (cond ((< n m) 0)
          (else (add1 (div (- n m) m))))))

(test 3 (div 15 4))
(test 3 (div 6 2))

;; pg 76

(define llength
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (llength (cdr l)))))))

(test 0 (llength '()))
(test 1 (llength '(a)))
(test 3 (llength '(a '(b c) d)))

(define pick
  (lambda (n lat)
    (cond ((= 1 n) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(test 'd (pick 4 '(a b c d e)))

;; pg 77

(define rempick
  (lambda (n lat)
    (cond ((= 1 n) (cdr lat))
          (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(test '(a b d) (rempick 3 '(a b c d)))

(define no-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat)) (no-nums (cdr lat)))
          (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond ((null? lat) '())
          ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
          (else (all-nums (cdr lat))))))

(test '(a b c) (no-nums '(1 a 2 b 3 c 4)))
(test '(1 2 3) (all-nums '(a 1 b 2 c 3 d)))

;; pg 78
(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (= a1 a2))
          ((and (atom? a1) (atom? a2)) (eq? a1 a2))
          (else #f))))

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

(test 0 (occur 'z '(a b c)))
(test 2 (occur 2 '(1 2 3 2 4)))

;; pg 79 is stupid and I basically already did it
