#lang racket/base

(provide eqan? ** pick div)

(require "lib/shared.rkt")

(module+ test
  (require rackunit))

;;; Chapter 4
;; pg 59

(module+ test
  (check-equal? (add1 67)
                68)
  (check-equal? (sub1 69)
                68)
  (check-false (zero? 42))
  (check-true (zero? 0)))

(define ++
  (lambda (m n)
    (cond [(zero? n) m]
          [else (++ (add1 m) (sub1 n))])))

(module+ test
  (check-equal? (++ 3 4)
                7)
  (check-equal? (++ 4 3)
                7))

;; pg 61

(define --
  (lambda (m n)
    (cond [(zero? n) m]
          [else (-- (sub1 m) (sub1 n))])))

(module+ test
  (check-equal? (-- 7 3)
                4))

(define tup?
  (lambda (l)
    (cond [(null? l) #t]
          [(number? (car l)) (tup? (cdr l))]
          [else #f])))

(module+ test
  (check-true (tup? '(1 2 3)))
  (check-false (tup? '(1 b 3)))
  (check-false (tup? '(1 (2 3) 4))))

(define addtup
  (lambda (l)
    (cond [(null? l) 0]
          [else (+ (car l) (addtup (cdr l)))])))

(module+ test
  (check-equal? (addtup '())
                0)
  (check-equal? (addtup '(1))
                1)
  (check-equal? (addtup '(1 2 3))
                6))

(define **
  (lambda (m n)
    (cond
     ;; ((< n m) (** n m))
     [(zero? m) 0]
     ;; ((zero? n) 0)
     ;; ((equal? 1 m) n)
     ;; ((equal? 1 n) m)
     [else (+ n (** n (sub1 m)))])))

(module+ test
  (check-equal? (** 5 0)
                0)
  (check-equal? (** 0 5)
                0)
  (check-equal? (** 1 5)
                5)
  (check-equal? (** 5 1)
                5)
  (check-equal? (** 2 3)
                6)
  (check-equal? (** 3 2)
                6))

(define tup+
  (lambda (t1 t2)
    (cond [(and (null? t1) (null? t2)) '()]
          [(null? t1) t2]
          [(null? t2) t1]
          [else (cons (+ (car t1) (car t2))
                      (tup+ (cdr t1) (cdr t2)))])))

(module+ test
  (check-equal? (tup+ '(3 6 9 11 4) '(8 5 2 0 7))
                '(11 11 11 11 11))
  (check-equal? (tup+ '(1 2 3) '(1 2 3 4 5))
                '(2 4 6 4 5))
  (check-equal? (tup+ '(1 2 3 4 5) '(1 2 3))
                '(2 4 6 4 5)))

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

(module+ test
  (check-true (== 3 3))
  (check-false (== 1 2))
  (check-false (== 2 1)))

;; pg 74

(define ^^
  (lambda (n exp)
    (cond [(zero? exp) 1]
          [(== 1 exp) n]
          [else (** n (^^ n (sub1 exp)))])))

(module+ test
  (check-equal? (^^ 5 0)
                1)
  (check-equal? (^^ 1 1)
                1)
  (check-equal? (^^ 2 3)
                8)
  (check-equal? (^^ 5 3)
                125))

(define div
  (lambda (n m)
    (cond [(< n m) 0]
          [else (add1 (div (- n m) m))])))

(module+ test
  (check-equal? (div 15 4)
                3)
  (check-equal? (div 6 2)
                3))

;; pg 76

(define llength
  (lambda (l)
    (cond [(null? l) 0]
          [else (add1 (llength (cdr l)))])))

(module+ test
  (check-equal? (llength '())
                0)
  (check-equal? (llength '(a))
                1)
  (check-equal? (llength '(a '(b c) d))
                3))

(define pick
  (lambda (n lat)
    (cond [(= 1 n) (car lat)]
          [else (pick (sub1 n) (cdr lat))])))

(module+ test
  (check-equal? (pick 4 '(a b c d e))
                'd))

;; pg 77

(define rempick
  (lambda (n lat)
    (cond [(= 1 n) (cdr lat)]
          [else (cons (car lat)
                      (rempick (sub1 n) (cdr lat)))])))

(module+ test
  (check-equal? (rempick 3 '(a b c d))
                '(a b d)))

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

(module+ test
  (check-equal? (no-nums '(1 a 2 b 3 c 4))
                '(a b c))
  (check-equal? (all-nums '(a 1 b 2 c 3 d))
                '(1 2 3)))

;; pg 78
(define eqan?
  (lambda (a1 a2)
    (cond [(and (number? a1) (number? a2)) (= a1 a2)]
          [(and (atom? a1) (atom? a2)) (eq? a1 a2)]
          [else #f])))

(module+ test
  (check-false (eqan? 1 2))
  (check-true (eqan? 2 2))
  (check-true (eqan? 'a 'a))
  (check-false (eqan? 'a 'b))
  (check-false (eqan? 1 '(why))))

(define occur
  (lambda (a lat)
    (cond [(null? lat) 0]
          [(eqan? a (car lat)) (add1 (occur a (cdr lat)))]
          [else (occur a (cdr lat))])))

(module+ test
  (check-equal? (occur 'z '(a b c))
                0)
  (check-equal? (occur 2 '(1 2 3 2 4))
                2))

;; pg 79 is stupid and I basically already did it
