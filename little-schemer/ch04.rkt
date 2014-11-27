#lang racket/base

(provide eqan? ** pick div)

(require "lib/shared.rkt")

;;; Chapter 4
;; pg 59

(test (add1 67)
      68)
(test (sub1 69)
      68)
(test (zero? 42)
      #f)
(test (zero? 0)
      #t)

(define ++
  (lambda (m n)
    (cond [(zero? n) m]
          [else (+ (add1 m) (sub1 n))])))

(test (++ 3 4)
      7)

;; pg 61

(define --
  (lambda (m n)
    (cond [(zero? n) m]
          [else (-- (sub1 m) (sub1 n))])))

(test (-- 7 3)
      4)

(define tup?
  (lambda (l)
    (cond [(null? l) #t]
          [(number? (car l)) (tup? (cdr l))]
          [else #f])))

(test (tup? '(1 2 3))
      #t)
(test (tup? '(1 b 3))
      #f)
(test (tup? '(1 (2 3) 4))
      #f)

(define addtup
  (lambda (l)
    (cond [(null? l) 0]
          [else (+ (car l) (addtup (cdr l)))])))

(test (addtup '())
      0)
(test (addtup '(1))
      1)
(test (addtup '(1 2 3))
      6)

(define **
  (lambda (m n)
    (cond
     ;; ((< n m) (** n m))
     [(zero? m) 0]
     ;; ((zero? n) 0)
     ;; ((equal? 1 m) n)
     ;; ((equal? 1 n) m)
     [else (+ n (** n (sub1 m)))])))

(test (** 5 0)
      0)
(test (** 0 5)
      0)
(test (** 1 5)
      5)
(test (** 5 1)
      5)
(test (** 2 3)
      6)
(test (** 3 2)
      6)

(define tup+
  (lambda (t1 t2)
    (cond [(and (null? t1) (null? t2)) '()]
          [(null? t1) t2]
          [(null? t2) t1]
          [else (cons (+ (car t1) (car t2))
                      (tup+ (cdr t1) (cdr t2)))])))

(test (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
      '(11 11 11 11 11))
(test (tup+ '(1 2 3) '(1 2 3 4 5))
      '(2 4 6 4 5))
(test (tup+ '(1 2 3 4 5) '(1 2 3))
      '(2 4 6 4 5))

;; pg 73
(define >>
  (lambda (n m)
    (cond [(zero? n) #f]
          [(zero? m) #t]
          [else (>> (sub1 n) (sub1 m))])))

(define <<
  (lambda (n m)
    (cond [(zero? m) #f]
          [(zero? n) #t]
          [else (<< (sub1 n) (sub1 m))])))

(define ==
  (lambda (n m)
    (not (or (<< n m) (>> n m)))))

(test (== 3 3)
      #t)
(test (== 1 2)
      #f)
(test (== 2 1)
      #f)

;; pg 74

(define ^^
  (lambda (n exp)
    (cond [(zero? exp) 1]
          [(== 1 exp) n]
          [else (** n (^^ n (sub1 exp)))])))

(test (^^ 1 1)
      1)
(test (^^ 2 3)
      8)
(test (^^ 5 3)
      125)

(define div
  (lambda (n m)
    (cond [(< n m) 0]
          [else (add1 (div (- n m) m))])))

(test (div 15 4)
      3)
(test (div 6 2)
      3)

;; pg 76

(define llength
  (lambda (l)
    (cond [(null? l) 0]
          [else (add1 (llength (cdr l)))])))

(test (llength '())
      0)
(test (llength '(a))
      1)
(test (llength '(a '(b c) d))
      3)

(define pick
  (lambda (n lat)
    (cond [(= 1 n) (car lat)]
          [else (pick (sub1 n) (cdr lat))])))

(test (pick 4 '(a b c d e))
      'd)

;; pg 77

(define rempick
  (lambda (n lat)
    (cond [(= 1 n) (cdr lat)]
          [else (cons (car lat)
                      (rempick (sub1 n) (cdr lat)))])))

(test (rempick 3 '(a b c d))
      '(a b d))

(define no-nums
  (lambda (lat)
    (cond [(null? lat) '()]
          [(number? (car lat)) (no-nums (cdr lat))]
          [else (cons (car lat)
                      (no-nums (cdr lat)))])))

(define all-nums
  (lambda (lat)
    (cond [(null? lat) '()]
          [(number? (car lat)) (cons (car lat)
                                     (all-nums (cdr lat)))]
          [else (all-nums (cdr lat))])))

(test (no-nums '(1 a 2 b 3 c 4))
      '(a b c))
(test (all-nums '(a 1 b 2 c 3 d))
      '(1 2 3))

;; pg 78
(define eqan?
  (lambda (a1 a2)
    (cond [(and (number? a1) (number? a2)) (= a1 a2)]
          [(and (atom? a1) (atom? a2)) (eq? a1 a2)]
          [else #f])))

(define occur
  (lambda (a lat)
    (cond [(null? lat) 0]
          [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
          [else (occur a (cdr lat))])))

(test (occur 'z '(a b c))
      0)
(test (occur 2 '(1 2 3 2 4))
      2)

;; pg 79 is stupid and I basically already did it
