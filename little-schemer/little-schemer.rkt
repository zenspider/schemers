#lang racket/base

(require "../sicp/lib/test.rkt")

(require "ch02.rkt")                    ; member?
(require "ch03.rkt")                    ; multirember & firsts
(require "ch04.rkt")                    ; eqan? ** pick div
(require "ch06.rkt")                    ; operator 1st-sub-exp 2nd-sub-exp

;;; Chapter 7
;; pg 111

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(test #f (set? '(a b a c)))
(test #t (set? '(a b c d)))
(test #t (set? '()))
(test #f (set? '(apple 3 pear 4 9 apple 3 4)))
(test #t (set? '(apple 3 pear 4 9)))

;; pg 112

(define makeset1
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset1 (cdr lat)))
     (else (cons (car lat) (makeset1 (cdr lat)))))))

(test '(c d a e b) (makeset1 '(a b c b d a e b)))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(test '(a b c d e) (makeset '(a b c b d a e b)))

;; pg 113

(test '(a 3 p 4 9) (makeset '(a 3 p 4 9 a 3 4)))

(define subset1?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset1? (cdr set1) set2))
     (else #f))))

(test #t (subset1? '(5 c w) '(5 h 2 p f c a l d w)))
(test #f (subset1? '(4 p o h) '(4 p c a 5 oz h)))

;; pg 114

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(test #t (eqset? '(6 l c wi w) '(6 c wi l w)))

;; pg 115

(define intersect1?
  (lambda (set1 set2)
    (cond
     ((or (null? set1) (null? set2)) #f)
     (else
      (or (member? (car set1) set2)
          (intersect1? (cdr set1) set2))))))

;; I don't know how to write this... does scheme even have specal forms like this?
;; (define nor
;;   (lambda (&rest args)
;;     (and (not (car args))
;;          (nor (cdr args)))))
;; (nor #t #f)

(define intersect?
  (lambda (set1 set2)
    (and
     (not (null? set1))
     (not (null? set2))
     (or (member? (car set1) set2)
         (intersect? (cdr set1) set2)))))

(test #t (intersect? '(a b c d) '(d c e)))
(test #f (intersect? '(a b c) '(d e f)))
(test #f (intersect? '() '(d c e)))
(test #f (intersect? '(d c e) '()))

;; pg 116

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(test '(and macaroni)
              (intersect '(stewed tomatoes and macaroni casserole)
                         '(macaroni and cheese)))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))

(test '(stewed tomatoes casserole macaroni and cheese)
              (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese)))

;; pg 117

(define difference
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (difference (cdr set1) set2))
     (else (cons (car set1) (difference (cdr set1) set2))))))

(test '(b d) (difference '(a b c d e) '(a c e)))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set)
                           (intersectall (cdr l-set)))))))

(test '(a) (intersectall '((a b c) (c a d e) (e f g h a b))))
(test '(6 and) (intersectall '((6 pears and)
                                       (3 peaches and 6 peppers)
                                       (8 pears and 6 plums)
                                       (and 6 prunes with some apples))))

;; pg 118

(define a-pair?
  (lambda (x)
    (cond ((atom? x) #f)
          ((null? x) #f)
          ((null? (cdr x)) #f)
          ((null? (cdr (cdr x))) #t)
          (else #f))))

(test #t (a-pair? '(full (house))))

;; pg 119

(define first car)
(define second cadr)
(define build (lambda (s1 s2) (cons s1 (cons s2 null))))
(define third caddr)

;; pg 120

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(test #t (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))

(define revrel1
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel))
                        (first  (car rel)))
                 (revrel1 (cdr rel)))))))

(test '((3 8) (2 4) (6 7) (2 6) (4 3))
              (revrel1 '((8 3) (4 2) (7 6) (6 2) (3 4))))

;; pg 121

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

(test '((3 8) (2 4) (6 7) (2 6) (4 3))
              (revrel '((8 3) (4 2) (7 6) (6 2) (3 4))))

;; pg 122

;; (define seconds
;;   (lambda (l)
;;     (cond ((null? l) '())
;;           (else (cons (cadr l) (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (and (fun? fun)
         (set? (revrel fun)))))

(test #t (fullfun? '((grape raisin) (plum prune) (stewed grape))))

;;; Chapter 8
;; pg 125-126

(define rember-f1
  (lambda (test? s l)
    (cond
     ((null? l) '())
     ((test? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember-f1 test? s (cdr l)))))))

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

(test #t ((eq?-c 'salad) 'salad))
(test #f ((eq?-c 'salad) 'pie))

(define eq?-salad (eq?-c 'salad))

(test #t (eq?-salad 'salad))
(test #f (eq?-salad 'pie))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
                   ((rember-f test?) a (cdr l))))))))

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
      (cond ((null? lat) '())
            ((test? (car lat) old) (cons new (cons old (cdr lat))))
            (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((test? (car lat) old) (cons old (cons new (cdr lat))))
            (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

(test '(a b c z d e)       ((insertR-f eq?) 'z 'c '(a b c d e)))
(test '(a b c d e f g d h) ((insertR-f eq?) 'e 'd '(a b c d f g d h)))
(test '(a b z c d e)       ((insertL-f eq?) 'z 'c '(a b c d e)))
(test '(a b c e d f g d h) ((insertL-f eq?) 'e 'd '(a b c d f g d h)))

(define insertX-f
  (lambda (match!)
    (lambda (test?)
      (lambda (new old lat)
        (cond ((null? lat) '())
              ((test? (car lat) old) (match! new old (cdr lat)))
              (else (cons (car lat)
                          (((insertX-f match!) test?) new old (cdr lat)))))))))

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
    (cond ((eq? x '+) +)
          ((eq? x '*) *)
          (else **))))

(define value4
  (lambda (exp)
    (cond
     ((atom? exp) exp)
     (else
      ((atom-to-function (operator exp))
       (value4 (1st-sub-exp exp))
       (value4 (2nd-sub-exp exp)))))))

(test #t (eq? 4 (value4 '(+ 1 3))))
(test #t (eq? 13 (value4 '(+ 1 (* 3 4)))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? (car lat) a)  ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(test '(a c d e) (multirember 'b '(b a b c b d b e b)))

;; pg 137

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat) (col null null))
          ((eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat (cons (car lat) seen)))))
          (else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat) seen)))))))

;; pg 138

(define a-friend (lambda (x y) (null? y)))

(test #t (multirember&co 'tuna '()                                a-friend))
(test #f (multirember&co 'tuna '(tuna)                            a-friend))
(test #f (multirember&co 'tuna '(strawberries tuna and swordfish) a-friend))

;; pg 141

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL)
           (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? (car lat) oldR)
           (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) '())
          ((eq? (car lat) oldL)
           (cons new
                 (cons oldL
                       (multiinsertLR&co new oldL oldR (cdr lat)
                                         (lambda (newlat L R)
                                           (col (cons new (cons oldL newlat))
                                                (add1 L) R))))))
          ((eq? (car lat) oldR)
           (cons oldR
                 (cons new
                       (multiinsertLR&co new oldL oldR (cdr lat)
                                         (lambda (newlat L R)
                                           (col (cons oldR (cons new newlat))
                                                L (add1 R)))))))
          (else (cons (car lat)
                      (multiinsertLR&co new oldL oldR (cdr lat)
                                        (lambda (newlat L R)
                                          (col (cons (car lat) newlat)
                                               L R))))))))

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
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(test '((2 8) 10 (() 6) 2) (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

;; fuck it... moving on to the next section.

(define keep-looking
  (lambda (a sorn lat)
    (cond ((number? sorn)
           (keep-looking a (pick sorn lat) lat))
          (else (eq? sorn a)))))

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
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (+ (length* (first pora))
                   (length* (second pora)))))))

(test 2 (length* '(1 2)))
(test 3 (length* '(1 (2 3))))
(test 4 (length* '((1 2) (3 4))))
(test 4 (length* '((1 2 3) (4 5 6))))   ; seems useless

;; pg 154

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
          (else (+ (* (weight* (first pora)) 2)
                   (weight* (second pora)))))))

(test 7 (weight* '((a b) c)))
(test 5 (weight* '(a (b c))))

(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

(test '(a (b c)) (shuffle '(a (b c))))
(test '(a b)     (shuffle '(a b)))
(test '(b a)     (revpair '(a b)))

(define C
  (lambda (n)
    (cond ((= 1 n) 1)
          (else (cond ((even? n) (C (div n 2)))
                      (else (C (add1 (* 3 n)))))))))

(test 1 (C 1))
(test 1 (C 2))
(test 1 (C 3))
(test 1 (C 4))

;; pg 156

(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
          ((zero? m) (A (sub1 n) 1))
          (else (A (sub1 n) (A n (sub1 m)))))))

(test 2 (A 1 0))
(test 3 (A 1 1))
(test 7 (A 2 2))

