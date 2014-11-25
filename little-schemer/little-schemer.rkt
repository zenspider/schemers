#lang racket/base

(require "../sicp/lib/test.rkt")

(require "ch02.rkt")

;;; Chapter 3
;; pg 33 - 42

(define myrember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (myrember a (cdr lat)))))))

(define rember1
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a) (cdr lat))
            (else (cons (car lat)
                        (rember1 a (cdr lat)))))))))

(test '(a c) (myrember 'b '(a b c)))
(test '(a b c) (myrember 'd '(a b c)))
(test '(a c) (rember1  'b '(a b c)))
(test '(a b c) (rember1  'd '(a b c)))

;; pg 43 - 46

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (caar l) (firsts (cdr l)))))))

(test '(a c e) (firsts '((a b) (c d) (e f))))
(test #t (null? (firsts '())))
(test '(a c d) (firsts '((a b) (c) (d e f))))

;; pg 47

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(test '(a b c z d e)       (insertR 'z 'c '(a b c d e)))
(test '(a b c d e f g d h) (insertR 'e 'd '(a b c d f g d h)))

;; pg 51

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cons old (cdr lat))))
          (else (cons (car lat) (insertL new old (cdr lat)))))))

(test '(a b z c d e)
              (insertL 'z 'c '(a b c d e)))
(test '(a b c e d f g d h)
              (insertL 'e 'd '(a b c d f g d h)))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(test '(a z c) (subst 'z 'b '(a b c)))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a)    (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(test '(a c d e) (multirember 'b '(b a b c b d b e b)))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(test '(a b z c b z d b z) (multiinsertR 'z 'b '(a b c b d b)))

;;; Chapter 4
;; pg 59

(define add1 (lambda (n) (+ n 1))) ; defined in intermediate?
(define sub1 (lambda (n) (- n 1))) ; defined in intermediate?

(test 68 (add1 67))
(test 68 (sub1 69))
(test #f (zero? 42))
(test #t (zero? 0))

;; (define +
;;   (lambda (m n)
;;     (cond ((zero? n) m)
;;           (else (+ (add1 m) (sub1 n))))))

;; (equal? 7 (+ 3 4))

;; pg 61

;; (define --
;;   (lambda (m n)
;;     (cond ((zero? n) m)
;;           (else (-- (sub1 m) (sub1 n))))))

;; (equal? 4 (-- 7 3))

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

;;; Chapter 5
;; pg 81

(define myrember*
  (lambda (a l)
    (cond ((null? l) '())
          ((pair? (car l)) (cons (myrember* a (car l)) (myrember* a (cdr l)))) ; "better"
          ((eq? a (car l)) (myrember* a (cdr l)))
          (else (cons (car l) (myrember* a (cdr l)))))))

(define rember*
  (lambda (a l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eq? (car l) a) (rember* a (cdr l)))
                 (else (cons (car l) (rember* a (cdr l))))))
          (else (cons (rember* a (car l)) (rember* a (cdr l))))))) ; appreciated on end

(test '()                  (myrember* 'a '()))
(test '(b c d)             (myrember* 'a '(a b a c d a)))
(test '((b) ((c)) (d (e))) (myrember* 'a '((b) a ((c) a) (d (e)) a)))
(test '()                  (rember* 'a '()))
(test '(b c d)             (rember* 'a '(a b a c d a)))
(test '((b) ((c)) (d (e))) (rember* 'a '((b) a ((c) a) (d (e)) a)))

;; pg 82
(define insertR*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eq? (car l) old)
                  (cons old (cons new (insertR* new old (cdr l)))))
                 (else (cons (car l) (insertR* new old (cdr l))))))
          (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define myinsertR*
  (lambda (new old l)
    (cond ((null? l) '())
          ((list? (car l)) (cons (myinsertR* new old (car l)) (myinsertR* new old (cdr l))))
          ((eq? old (car l)) (cons old (cons new (myinsertR* new old (cdr l)))))
          (else (cons (car l) (myinsertR* new old (cdr l)))))))

(test '()              (myinsertR* 'a 'b '()))
(test '(a b (a b (c))) (myinsertR* 'b 'a '(a (a (c)))))
(test '()              (insertR* 'a 'b '()))
(test '(a b (a b (c))) (insertR* 'b 'a '(a (a (c)))))

;; pg 84
(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((list? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
          ((eq? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l))))))

(test 0 (occur* 'a '(b c d)))
(test 1 (occur* 'a '(((a)))))
(test 3 (occur* 'a '(1 a 2 (3 a 4 (5)) a)))

;; pg 85
(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
          ((eq? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l)))))))

(test '(a z c) (subst* 'z 'b '(a b c)))
(test '((a) (z ((((c d))) e (f)) z) (h) (z) (j k))
              (subst* 'z 'b
                      '((a) (b ((((c d))) e (f)) b) (h) (b) (j k))))

;; pg 86
(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
          ((list? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
          ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l)))))))

(test '(z a b (z a b (z a z a) c) c c)
              (insertL* 'z 'a '(a b (a b (a a) c) c c)))

;; pg 87
(define member1*
  (lambda (a l)
    (not (eq? 0 (occur* a l)))))

(define member2*
  (lambda (a l)
    (cond ((null? l) #f)
          ((list? (car l)) (or (member2* a (car l)) (member2* a (cdr l))))
          ((eq? a (car l)) #t)
          (else (member2* a (cdr l))))))

(test #t  (member1* 'b '((a (b)) c)))
(test #f (member1* 'z '((a (b)) c)))
(test #t  (member2* 'b '((a (b)) c)))
(test #f (member2* 'z '((a (b)) c)))

;; pg 88
(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((list? (car l)) (leftmost (car l)))
          (else (car l)))))

(test 'a (leftmost '((a) (b ((c) d) (e)))))
(test 'a (leftmost '(((a) (b (c))) d)))
(test '() (leftmost '(((() a)) b (c))))

;; pg 90
(define myeqlist1?
  (lambda (a b)
    (cond
     ((and (null? a) (null? b)) #t)
     ((or (null? a) (null? b)) #f)
     ((and (list? (car a)) (list? (car b)))
      (and (myeqlist1? (car a) (car b))
           (myeqlist1? (cdr a) (cdr b))))
     ((and (atom? (car a)) (atom? (car b)))
      (and (eq? (car a) (car b))
           (myeqlist1? (cdr a) (cdr b))))
     (else #f))))

;; HACK (define myeqlist? myeqlist1?)

(test #t (myeqlist1? '() '()))
(test #t (myeqlist1? '(a b c) '(a b c)))
(test #t (myeqlist1? '(a (b) c) '(a (b) c)))
(test #f (myeqlist1? '(a b c) '(a b)))
(test #f (myeqlist1? '(a b c) '(a (b) c)))

;; pg 91
(define myeqlist2?
  (lambda (a b)
    (cond
     ((and (null? a) (null? b)) #t)
     ((and (null? a) (atom? (car b))) #f)
     ((null? a) #f)
     ((and (atom? (car a)) (null? b)) #f)
     ((and (atom? (car a)) (atom? (car b)))
      (and (eqan? (car a) (car b))
           (myeqlist2? (cdr a) (cdr b))))
     ((atom? (car a)) #f)
     ((null? b) #f)
     (else
      (and (myeqlist2? (car a) (car b))
           (myeqlist2? (cdr a) (cdr b)))))))

;; HACK (define myeqlist? myeqlist2?)

(test #t (myeqlist2? '() '()))
(test #t (myeqlist2? '(a b c) '(a b c)))
(test #t (myeqlist2? '(a (b) c) '(a (b) c)))
(test #f (myeqlist2? '(a b c) '(a b)))
(test #f (myeqlist2? '(a b c) '(a (b) c)))

;; pg 92 - 93

(define myeqlist3?
  (lambda (a b)
    (cond
     ((and (null? a) (null? b)) #t)
     ((or (null? a) (null? b)) #f)
     ((and (atom? (car a)) (atom? (car b)))
      (and (eqan? (car a) (car b))
           (myeqlist3? (cdr a) (cdr b))))
     ((or (atom? (car a)) (atom? (car b))) #f)
     (else
      (and (myeqlist3? (car a) (car b))
           (myeqlist3? (cdr a) (cdr b)))))))

;; HACK (define myeqlist? myeqlist3?)

(test #t (myeqlist3? '() '()))
(test #t (myeqlist3? '(a b c) '(a b c)))
(test #t (myeqlist3? '(a (b) c) '(a (b) c)))
(test #f (myeqlist3? '(a b c) '(a b)))
(test #f (myeqlist3? '(a b c) '(a (b) c)))

(define myequal?
  (lambda (a b)
    (cond
     ((and (atom? a) (atom? b)) (eqan? a b))
     ((or  (atom? a) (atom? b)) #f)
     (else (myeqlist3? a b)))))

;; version with eqan inlined:
;; (define myequal?
;;   (lambda (a b)
;;     (cond
;;      ((and (atom? a) (atom? b))
;;       (cond ((and (number? a) (number? b)) (= a b))
;;             (else (eq? a b))))
;;      ((or  (atom? a) (atom? b)) #f)
;;      (else (myeqlist? a b)))))


(test #t (myequal? 'a 'a))
(test #t (myequal? '() '()))
(test #f (myequal? 'a 'b))
(test #f (myequal? 'b 'a))
(test #f (myequal? 'a '()))
(test #f (myequal? '() 'a))
(test #t (myequal? '(a (b) c) '(a (b) c)))
(test #f (myequal? '(a b c) '(a b)))
(test #f (myequal? '(a b c) '(a (b) c)))

(define myeqlist?
  (lambda (a b)
    (cond
     ((and (null? a) (null? b)) #t)
     ((or (null? a) (null? b)) #f)
     (else
      (and (equal? (car a) (car b))
           (myeqlist? (cdr a) (cdr b)))))))

;; pg 94
(define rember2
  (lambda (s l)
    (cond
     ((null? l) '())
     (else
      (cond
       ((equal? (car l) s) (cdr l))
       (else
        (cons (car l)
              (rember2 s (cdr l)))))))))

;; pg 95

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s (cdr l)))))))

;;; Chapter 6
;; pg 97 - 99

(define numbered1?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+) #t)
     ((eq? (car (cdr aexp)) '*) #t)
     ((eq? (car (cdr aexp)) '^) #t))))

;; pg 100 - 101

(define numbered2
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered2 (car aexp))
           (numbered2 (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '*)
      (and (numbered2 (car aexp))
           (numbered2 (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '^)
      (and (numbered2 (car aexp))
           (numbered2 (car (cdr (cdr aexp)))))))))

;; lame version - doesn't ask about op
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

;; pg 102 - 103
(define value1
  (lambda (exp)
    (cond
     ((atom? exp) exp)
     ((eq? (car (cdr exp)) '+)
      (+ (value1 (car exp))
         (value1 (car (cdr (cdr exp))))))
     ((eq? (car (cdr exp)) '*)
      (* (value1 (car exp))
         (value1 (car (cdr (cdr exp))))))
     ((eq? (car (cdr exp)) '^)
      (** (value1 (car exp))
          (value1 (car (cdr (cdr exp)))))))))

(test #t (eq? 4 (value1 '(1 + 3))))
(test #t (eq? 13 (value1 '(1 + (3 * 4)))))

;; pg 104 - 105

(define value2
  (lambda (exp)
    (cond
     ((atom? exp) exp)
     ((eq? (car exp) '+)
      (+ (value2 (car (cdr exp)))
         (value2 (car (cdr (cdr exp))))))
     ((eq? (car exp) '*)
      (* (value2 (car (cdr exp)))
         (value2 (car (cdr (cdr exp))))))
     ((eq? (car exp) '^)
      (** (value2 (car (cdr exp)))
          (value2 (car (cdr (cdr exp)))))))))

(test #t (eq? 4 (value2 '(+ 1 3))))
(test #t (eq? 13 (value2 '(+ 1 (* 3 4)))))

(define 1st-sub-exp
  (lambda (exp)
    (car (cdr exp))))

;; pg 106

(define 2nd-sub-exp
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define operator
  (lambda (exp)
    (car exp)))

(define value3
  (lambda (exp)
    (cond
     ((atom? exp) exp)
     ((eq? (operator exp) '+)
      (+ (value3 (1st-sub-exp exp))
         (value3 (2nd-sub-exp exp))))
     ((eq? (operator exp) '*)
      (* (value3 (1st-sub-exp exp))
         (value3 (2nd-sub-exp exp))))
     ((eq? (operator exp) '^)
      (** (value3 (1st-sub-exp exp))
          (value3 (2nd-sub-exp exp)))))))

(test #t (eq? 4 (value3 '(+ 1 3))))
(test #t (eq? 13 (value3 '(+ 1 (* 3 4)))))

;; pg 107

(define sero?
  (lambda (n)
    (null? n)))

(test #t (sero? '()))
(test #f (sero? 4))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define pluz
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else
      (edd1 (pluz n (zub1 m)))))))

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

