#lang racket/base

(require "lib/shared.rkt")

(require "ch03.rkt")                    ; multirember & firsts
(require "ch04.rkt")                    ; eqan? ** pick div
(require "ch06.rkt")                    ; operator 1st-sub-exp 2nd-sub-exp
(require "ch07.rkt")                    ; build first second a-pair? revpair

;;; Chapter 8
;; pg 125-126

(define rember-f1
  (lambda (test? s l)
    (cond
     [(null? l) '()]
     [(test? (car l) s) (cdr l)]
     [else (cons (car l)
                 (rember-f1 test? s (cdr l)))])))

(test '(6 2 3)
              (rember-f1 = 5 '(6 2 5 3)))
(test '(beans are good)
              (rember-f1 eq? 'jelly '(jelly beans are good)))
(test '(lemonade and (cake))
              (rember-f1 equal? '(pop corn) '(lemonade (pop corn) and (cake))))

;; pg 127-129

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

[test #t ((eq?-c 'salad) 'salad)]
(test #f ((eq?-c 'salad) 'pie))

(define eq?-salad (eq?-c 'salad))

(test #t (eq?-salad 'salad))
(test #f (eq?-salad 'pie))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) '()]
       [(test? (car l) a) (cdr l)]
       [else (cons (car l)
                   ((rember-f test?) a (cdr l)))]))))

(define rember-eq? (rember-f eq?))

(test '(salad is good) (rember-eq? 'tuna '(tuna salad is good)))
(test '(shrimp salad and salad) ((rember-f eq?) 'tuna
                                         '(shrimp salad and tuna salad)))

;; pg 130-133

(test '(equal? eqan? eqlist? eqpair?)
              ((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?)))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond [(null? lat) '()]
            [(test? (car lat) old) (cons new
                                         (cons old
                                               (cdr lat)))]
            [else (cons (car lat)
                        ((insertL-f test?) new old (cdr lat)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond [(null? lat) '()]
            [(test? (car lat) old) (cons old
                                         (cons new
                                               (cdr lat)))]
            [else (cons (car lat)
                        ((insertR-f test?) new old (cdr lat)))]))))

(test '(a b c z d e)       ((insertR-f eq?) 'z 'c '(a b c d e)))
(test '(a b c d e f g d h) ((insertR-f eq?) 'e 'd '(a b c d f g d h)))
(test '(a b z c d e)       ((insertL-f eq?) 'z 'c '(a b c d e)))
(test '(a b c e d f g d h) ((insertL-f eq?) 'e 'd '(a b c d f g d h)))

(define insertX-f
  (lambda (match!)
    (lambda (test?)
      (lambda (new old lat)
        (cond [(null? lat) '()]
              [(test? (car lat) old) (match! new old (cdr lat))]
              [else (cons (car lat)
                          (((insertX-f match!) test?) new old (cdr lat)))])))))

(define seqR (lambda (new old l) (cons old (cons new l))))
(define seqL (lambda (new old l) (cons new (cons old l))))

(define insertR-fm (insertX-f seqR))
(define insertL-fm (insertX-f seqL))

(test '(a b c z d e)       ((insertR-fm eq?) 'z 'c '(a b c d e)))
(test '(a b c d e f g d h) ((insertR-fm eq?) 'e 'd '(a b c d f g d h)))
(test '(a b z c d e)       ((insertL-fm eq?) 'z 'c '(a b c d e)))
(test '(a b c e d f g d h) ((insertL-fm eq?) 'e 'd '(a b c d f g d h)))

;; pg 134-135

(define atom-to-function
  (lambda (x)
    (cond [(eq? x '+) +]
          [(eq? x '*) *]
          [else **])))

(define value4
  (lambda (exp)
    (cond
     [(atom? exp) exp]
     [else
      ((atom-to-function (operator exp))
       (value4 (1st-sub-exp exp))
       (value4 (2nd-sub-exp exp)))])))

(test #t (eq? 4 (value4 '(+ 1 3))))
(test #t (eq? 13 (value4 '(+ 1 (* 3 4)))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond [(null? lat) '()]
            [(test? (car lat) a)
             ((multirember-f test?) a (cdr lat))]
            [else (cons (car lat)
                        ((multirember-f test?) a (cdr lat)))]))))

(test '(a c d e) (multirember 'b '(b a b c b d b e b)))

;; pg 137

(define multirember&co
  (lambda (a lat col)
    (cond [(null? lat) (col null null)]
          [(eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat (cons (car lat)
                                               seen))))]
          [else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat)
                                        newlat)
                                  seen)))])))

;; pg 138

(define a-friend (lambda (x y) (null? y)))

(test #t (multirember&co 'tuna '()                                a-friend))
(test #f (multirember&co 'tuna '(tuna)                            a-friend))
(test #f (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend))

;; pg 141

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond [(null? lat) '()]
          [(eq? (car lat) oldL)
           (cons new
                 (cons oldL
                       (multiinsertLR new oldL oldR (cdr lat))))]
          [(eq? (car lat) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR new oldL oldR (cdr lat))))]
          [else (cons (car lat)
                      (multiinsertLR new oldL oldR (cdr lat)))])))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond [(null? lat) '()]
          [(eq? (car lat) oldL)
           (cons new
                 (cons oldL
                       (multiinsertLR&co new oldL oldR (cdr lat)
                                         (lambda (newlat L R)
                                           (col (cons new
                                                      (cons oldL
                                                            newlat))
                                                (add1 L) R)))))]
          [(eq? (car lat) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR&co new oldL oldR (cdr lat)
                                         (lambda (newlat L R)
                                           (col (cons oldR
                                                      (cons new
                                                            newlat))
                                                L (add1 R))))))]
          [else (cons (car lat)
                      (multiinsertLR&co new oldL oldR (cdr lat)
                                        (lambda (newlat L R)
                                          (col (cons (car lat)
                                                     newlat)
                                               L R))))])))

;; I don't get where this is going at all... I'm gonna try to go
;; faster to get to the next section.

;; pg 144

(define even?
  (lambda (n)
    (= (remainder n 2) 0)))

(test #f (even? 3))
(test #t (even? 4))

(define evens-only*
  (lambda (l)
    (cond [(null? l) '()]
          [(atom? (car l))
           (cond [(even? (car l)) (cons (car l)
                                        (evens-only* (cdr l)))]
                 [else (evens-only* (cdr l))])]
          [else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))])))

(test '((2 8) 10 (() 6) 2) (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

;; fuck it... moving on to the next section.

(define keep-looking
  (lambda (a sorn lat)
    (cond [(number? sorn)
           (keep-looking a (pick sorn lat) lat)]
          [else (eq? sorn a)])))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(test #t (looking 'caviar '(6 2 4 caviar 5 7 3)))
(test #f (looking 'caviar '(6 2 grits caviar 5 7 3)))

;; pg 151

(define eternity
  (lambda (x) (eternity x)))

;; pg 152

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(test '(a (b c)) (shift '((a b) c)))
(test '(a (b (c d))) (shift '((a b) (c d))))

(define align
  (lambda (pora)
    (cond [(atom? pora) pora]
          [(a-pair? (first pora))
           (align (shift pora))]
          [else (build (first pora)
                       (align (second pora)))])))

(define length*
  (lambda (pora)
    (cond [(atom? pora) 1]
          [else (+ (length* (first pora))
                   (length* (second pora)))])))

(test 2 (length* '(1 2)))
(test 3 (length* '(1 (2 3))))
(test 4 (length* '((1 2) (3 4))))
(test 4 (length* '((1 2 3) (4 5 6))))   ; seems useless

;; pg 154

(define weight*
  (lambda (pora)
    (cond [(atom? pora) 1]
          [else (+ (* (weight* (first pora)) 2)
                   (weight* (second pora)))])))

(test 7 (weight* '((a b) c)))
(test 5 (weight* '(a (b c))))

(define shuffle
  (lambda (pora)
    (cond [(atom? pora) pora]
          [(a-pair? (first pora))
           (shuffle (revpair pora))]
          [else (build (first pora)
                       (shuffle (second pora)))])))

(test '(a (b c)) (shuffle '(a (b c))))
(test '(a b)     (shuffle '(a b)))
(test '(b a)     (revpair '(a b)))

(define C
  (lambda (n)
    (cond [(= 1 n) 1]
          [else (cond [(even? n) (C (div n 2))]
                      [else (C (add1 (* 3 n)))])])))

(test 1 (C 1))
(test 1 (C 2))
(test 1 (C 3))
(test 1 (C 4))

;; pg 156

(define A
  (lambda (n m)
    (cond [(zero? n) (add1 m)]
          [(zero? m) (A (sub1 n) 1)]
          [else (A (sub1 n) (A n (sub1 m)))])))

(test 2 (A 1 0))
(test 3 (A 1 1))
(test 7 (A 2 2))
