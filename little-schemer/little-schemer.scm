#lang racket

;;; Laws & Commandments:

;;; Laws:

;; pg 5: Car: The primitive car is defined only for non-empty lists.

;; pg 7: Cdr: The primitive cdr is defined only for non-empty lists.
;;            The cdr of any non-empty list is always another list.

;; pg 9: Cons: The primitive cons takes two arguments. The second
;;             argument to cons must be a list. The result is a list.

;; pg 10: Null?: The primitive null? is defined only for lists.

;; pg 12: Eq?: The primitive eq? takes two arguments. Each must be a
;;             non-numeric atom

;;; Commandments:

;; pg 23: 1st: (preliminary) Always ask null? as the first question in
;;             expressing any function.

;; pg 37: 2nd: Use cons to build lists

;; pg 45: 3rd: When building a list, describe the first typical
;;             element, and then cons it onto the natural recursion

;; pg 64: 1st: (revised) When recurring on a list of atoms, lat, ask
;;             two questions about it: (null? lat) and else. When
;;             recurring on a number, n, ask two questions about it:
;;             (zero? n) and else.

;; pg 65: 4th: (revised) Always change at least one argument while
;;             recurring. It must be changed to be closer to
;;             termination. The changing argument must be tested in
;;             the termination condition: when using cdr, test
;;             termination with null? and when using sub1, test
;;             termination with zero?.

;; pg 67: 5th: When building a value with +, always use 0 for the
;;             value of the terminating line, for adding 0 does not
;;             change the value of addition. When building a valuew
;;             ith &, always ues 1 for the value of the terminating
;;             line, for multiplying by 1 does not change the value of
;;             multiplication. When building a value with cons, always
;;             consider '() for the value of the terminating line.

;; pg 83: 1st: (final) When recurring on a list of atoms, lat, ask two
;;             questions about it: (null? lat) and else. When
;;             recurring on a number, n, ask two qustions about it:
;;             (zero? n) and else. When recurring on a list of sexps,
;;             l, ask 3 questions about it: (null? l), (atom? (car l))
;;             and else. [ I'm not fond of this one ]

;; pg 84: 4th: (final) Always change at least one argument while
;;             recurring. When recurring on a list of atems, lat, use
;;             (cdr lat). When recurring on a number, n, use (sub1 n).
;;             And when recurring on a sexp, l, use (car l) and (cdr
;;             l) if neither (null? l) nor (atom? (car l)) are true.
;;             It must be changed to be closer to termination. The
;;             changing argument must be tested in the cermination
;;             condition: when using cdr, test termination with null?
;;             and when using sub1, test termination with zero?.

;; pg 94: 6th: Simplify only after the function is correct.

;; pg 103: 7th: Recur on the subparts that are the same nature: on the
;;              sublists of a list, on the subexpression of an
;;              arithmetic expression.

;; pg 107: 8th: Use help functions to abstract from representations.

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define list?
  (lambda (x)
    (not (atom? x))))

;; scheme sanity check:
(not (atom? '()))

;;; Chapter 1
                                        ; pg 1 - 4
(and (atom? 'atom)
     (atom? 1492)
     (list? '(atom))
     (list? '(atom turkey or)))

                                        ; pg 5 - 7
(and (eq? (car '(a b c)) 'a)
     (equal? (car '((a b c) x y z)) '(a b c))
     (equal? (car (cdr '((b) (x y) ((c))))) '(x y))
     (equal? (cdr (cdr '((b) (x y) ((c))))) '(((c)))))
;; (car '()) ; error on pedantic scheme
;; (cdr '()) ; error on pedantic scheme
;; (cdr (car '(a (b (c)) d))) ; error on pedantic scheme (cdr 'a)

                                        ; pg 8
(and (equal? (cons '(banana and) '(peanut butter and jelly))
             '((banana and) peanut butter and jelly))
     (equal? (cons '((help) this) '(is very ((hard) to learn)))
             '(((help) this) is very ((hard) to learn)))
     (equal? (cons '(a b (c)) '())
             '((a b (c))))
     (equal? (cons 'a '()) '(a)))
;; (cons '((a b c)) 'b) ; INTERESTING: not an error in lisp
;; (cons 'a 'b) ; ditto

                                        ; pg 9

(and (equal? '(a b) (cons 'a (car '((b) c d))))
     (equal? '(a c d) (cons 'a (cdr '((b) c d))))
     (null? '())
     (not (null? '(a b c)))
     (not (null? 'spaghetti)))

                                        ; pg 10
(and (not (atom? '(harry had an apple)))
     (not (atom? '()))
     (atom? 42)
     (not (atom? (car (cdr '(swing (low sweet) cherry oat))))))

                                        ; pg 11 - 12
(and (eq? 'Harry (quote Harry))
     (not (eq? 'margerine 'butter))
     (not (eq? '() '(a)))
     (eq? 'mary (car '(mary had a little lamb)))
     (not (eq? (cdr '(soured milk)) 'milk))
     (eq? (car (cdr '(soured milk))) 'milk))

;;; Chapter 2
                                        ; pg 15 - 20

(define mylat?
  (lambda (x)
    (or (null? x)
        (and (list? x)
             (atom? (car x))
             (mylat? (cdr x))))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(and (mylat? '(a b c))
     (not (mylat? '((a) b c d)))
     (not (mylat? '(a (b) c d)))
     (mylat? '())
     (lat? '(a b c))
     (not (lat? '((a) b c d)))
     (not (lat? '(a (b) c d)))
     (lat? '()))

                                        ; pg 21 - 31

(and (or (null? '()) (atom? '(a b c)))
     (or (null? '(a b c)) (null? '())))

(define mymember?
  (lambda (a lat)
    (and (not (null? lat))
         (or (and (atom? (car lat)) (eq? a (car lat)))
             (mymember? a (cdr lat))))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))

(and (mymember? 'b '(a b c))
     (not (mymember? 'd '(a b c)))
     (member? 'b '(a b c))
     (not (member? 'd '(a b c))))

;;; Chapter 3
                                        ; pg 33 - 42

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

(and (equal? (myrember 'b '(a b c)) '(a c))
     (equal? (myrember 'd '(a b c)) '(a b c)))
(and (equal? (rember1  'b '(a b c)) '(a c))
     (equal? (rember1  'd '(a b c)) '(a b c)))

                                        ; pg 43 - 46

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (caar l) (firsts (cdr l)))))))

(and (equal? (firsts '((a b) (c d) (e f))) '(a c e))
     (null? (firsts '()))
     (equal? (firsts '((a b) (c) (d e f))) '(a c d)))

                                        ; pg 47

(define insertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons old (cons new (cdr lat))))
          (else (cons (car lat) (insertR new old (cdr lat)))))))

(and (equal? (insertR 'z 'c '(a b c d e))
             '(a b c z d e))
     (equal? (insertR 'e 'd '(a b c d f g d h))
             '(a b c d e f g d h)))

                                        ; pg 51

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cons old (cdr lat))))
          (else (cons (car lat) (insertL new old (cdr lat)))))))

(and (equal? (insertL 'z 'c '(a b c d e))
             '(a b z c d e))
     (equal? (insertL 'e 'd '(a b c d f g d h))
             '(a b c e d f g d h)))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old) (cons new (cdr lat)))
          (else (cons (car lat) (subst new old (cdr lat)))))))

(equal? (subst 'z 'b '(a b c)) '(a z c))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((eq? (car lat) a)    (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))

(equal? (multirember 'b '(b a b c b d b e b)) '(a c d e))

(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? (car lat) old)
           (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(equal? (multiinsertR 'z 'b '(a b c b d b)) '(a b z c b z d b z))

;;; Chapter 4
                                        ; pg 59

(define add1 (lambda (n) (+ n 1))) ; defined in intermediate?
(define sub1 (lambda (n) (- n 1))) ; defined in intermediate? 

(and (equal? 68 (add1 67))
     (equal? 68 (sub1 69))
     (not (zero? 42))
     (zero? 0))

;; (define +
;;   (lambda (m n)
;;     (cond ((zero? n) m)
;;           (else (+ (add1 m) (sub1 n))))))

;; (equal? 7 (+ 3 4))

                                        ; pg 61

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

(and (tup? '(1 2 3))
     (not (tup? '(1 b 3)))
     (not (tup? '(1 (2 3) 4))))

(define addtup
  (lambda (l)
    (cond ((null? l) 0)
          (else (+ (car l) (addtup (cdr l)))))))

(and (equal? 0 (addtup '()))
     (equal? 1 (addtup '(1)))
     (equal? 6 (addtup '(1 2 3))))

(define **
  (lambda (m n)
    (cond 
     ;; ((< n m) (** n m))
     ((zero? m) 0)
     ;; ((zero? n) 0)
     ;; ((equal? 1 m) n)
     ;; ((equal? 1 n) m)
     (else (+ n (** n (sub1 m)))))))

(and (equal? 0 (** 5 0))
     (equal? 0 (** 0 5))
     (equal? 5 (** 1 5))
     (equal? 5 (** 5 1))
     (equal? 6 (** 2 3))
     (equal? 6 (** 3 2)))

(define tup+
  (lambda (t1 t2)
    (cond ((and (null? t1) (null? t2)) '())
          ((null? t1) t2)
          ((null? t2) t1)
          (else (cons (+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

(and (equal? '(11 11 11 11 11) (tup+ '(3 6 9 11 4) '(8 5 2 0 7)))
     (equal? '(2 4 6 4 5) (tup+ '(1 2 3) '(1 2 3 4 5)))
     (equal? '(2 4 6 4 5) (tup+ '(1 2 3 4 5) '(1 2 3))))

                                        ; pg 73
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

(and (== 3 3)
     (not (== 1 2))
     (not (== 2 1)))

                                        ; pg 74

(define ^^
  (lambda (n exp)
    (cond ((zero? exp) 1)
          ((== 1 exp) n)
          (else (** n (^^ n (sub1 exp)))))))

(and (equal? 1 (^^ 1 1))
     (equal? 8 (^^ 2 3))
     (equal? 125 (^^ 5 3)))

(define div
  (lambda (n m)
    (cond ((< n m) 0)
          (else (add1 (div (- n m) m))))))

(and (equal? 3 (div 15 4))
     (equal? 3 (div 6 2)))

                                        ; pg 76

(define llength
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (llength (cdr l)))))))

(and (equal? 0 (llength '()))
     (equal? 1 (llength '(a)))
     (equal? 3 (llength '(a '(b c) d))))

(define pick
  (lambda (n lat)
    (cond ((= 1 n) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(equal? 'd (pick 4 '(a b c d e)))

                                        ; pg 77

(define rempick
  (lambda (n lat)
    (cond ((= 1 n) (cdr lat))
          (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(equal? '(a b d) (rempick 3 '(a b c d)))

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
      
(equal? '(a b c) (no-nums '(1 a 2 b 3 c 4)))
(equal? '(1 2 3) (all-nums '(a 1 b 2 c 3 d)))

                                        ; pg 78
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

(and (equal? 0 (occur 'z '(a b c)))
     (equal? 2 (occur 2 '(1 2 3 2 4))))

;; pg 79 is stupid and I basically already did it

;;; Chapter 5
                                        ; pg 81

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

(and (equal? '() (myrember* 'a '()))
     (equal? '(b c d) (myrember* 'a '(a b a c d a)))
     (equal? '((b) ((c)) (d (e))) (myrember* 'a '((b) a ((c) a) (d (e)) a)))
     (equal? '() (rember* 'a '()))
     (equal? '(b c d) (rember* 'a '(a b a c d a)))
     (equal? '((b) ((c)) (d (e))) (rember* 'a '((b) a ((c) a) (d (e)) a))))

                                        ; pg 82
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

(and (equal? '() (myinsertR* 'a 'b '()))
     (equal? '(a b (a b (c))) (myinsertR* 'b 'a '(a (a (c)))))
     (equal? '() (insertR* 'a 'b '()))
     (equal? '(a b (a b (c))) (insertR* 'b 'a '(a (a (c))))))

                                        ; pg 84
(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((list? (car l)) (+ (occur* a (car l)) (occur* a (cdr l))))
          ((eq? a (car l)) (add1 (occur* a (cdr l))))
          (else (occur* a (cdr l))))))

(and (equal? 0 (occur* 'a '(b c d)))
     (equal? 1 (occur* 'a '(((a)))))
     (equal? 3 (occur* 'a '(1 a 2 (3 a 4 (5)) a))))

                                        ; pg 85
(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((list? (car l)) (cons (subst* new old (car l)) (subst* new old (cdr l))))
          ((eq? old (car l)) (cons new (subst* new old (cdr l))))
          (else (cons (car l) (subst* new old (cdr l)))))))

(and (equal? '(a z c) (subst* 'z 'b '(a b c)))
     (equal? '((a) (z ((((c d))) e (f)) z) (h) (z) (j k))
             (subst* 'z 'b
                     '((a) (b ((((c d))) e (f)) b) (h) (b) (j k)))))

                                        ; pg 86
(define insertL*
  (lambda (new old l)
    (cond ((null? l) '())
          ((list? (car l)) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
          ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
          (else (cons (car l) (insertL* new old (cdr l)))))))

(equal? '(z a b (z a b (z a z a) c) c c)
        (insertL* 'z 'a '(a b (a b (a a) c) c c)))

                                        ; pg 87
(define member1*
  (lambda (a l)
    (not (eq? 0 (occur* a l)))))

(define member2*
  (lambda (a l)
    (cond ((null? l) #f)
          ((list? (car l)) (or (member2* a (car l)) (member2* a (cdr l))))
          ((eq? a (car l)) #t)
          (else (member2* a (cdr l))))))

(and (equal? #t (member1* 'b '((a (b)) c)))
     (equal? #f (member1* 'z '((a (b)) c)))
     (equal? #t (member2* 'b '((a (b)) c)))
     (equal? #f (member2* 'z '((a (b)) c))))

                                        ; pg 88
(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((list? (car l)) (leftmost (car l)))
          (else (car l)))))

(and (equal? 'a (leftmost '((a) (b ((c) d) (e)))))
     (equal? 'a (leftmost '(((a) (b (c))) d)))
     (equal? '() (leftmost '(((() a)) b (c)))))

                                        ; pg 90
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

(and (myeqlist1? '() '())
     (myeqlist1? '(a b c) '(a b c))
     (myeqlist1? '(a (b) c) '(a (b) c))
     (not (myeqlist1? '(a b c) '(a b)))
     (not (myeqlist1? '(a b c) '(a (b) c))))

                                        ; pg 91
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

(and (myeqlist2? '() '())
     (myeqlist2? '(a b c) '(a b c))
     (myeqlist2? '(a (b) c) '(a (b) c))
     (not (myeqlist2? '(a b c) '(a b)))
     (not (myeqlist2? '(a b c) '(a (b) c))))

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

(and (myeqlist3? '() '())
     (myeqlist3? '(a b c) '(a b c))
     (myeqlist3? '(a (b) c) '(a (b) c))
     (not (myeqlist3? '(a b c) '(a b)))
     (not (myeqlist3? '(a b c) '(a (b) c))))

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


(and (myequal? 'a 'a)
     (myequal? '() '())
     (not (myequal? 'a 'b))
     (not (myequal? 'b 'a))
     (not (myequal? 'a '()))
     (not (myequal? '() 'a))
     (myequal? '(a (b) c) '(a (b) c))
     (not (myequal? '(a b c) '(a b)))
     (not (myequal? '(a b c) '(a (b) c))))

(define myeqlist?
  (lambda (a b)
    (cond
     ((and (null? a) (null? b)) #t)
     ((or (null? a) (null? b)) #f)
     (else
      (and (equal? (car a) (car b))
           (myeqlist? (cdr a) (cdr b)))))))

                                        ; pg 94
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

                                        ; pg 95

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s (cdr l)))))))

;;; Chapter 6
                                        ; pg 97 - 99

(define numbered1?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+) #t)
     ((eq? (car (cdr aexp)) '*) #t)
     ((eq? (car (cdr aexp)) '^) #t))))

                                        ; pg 100 - 101

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

                                        ; pg 102 - 103
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

(and (eq? 4 (value1 '(1 + 3)))
     (eq? 13 (value1 '(1 + (3 * 4)))))

                                        ; pg 104 - 105

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

(and (eq? 4 (value2 '(+ 1 3)))
     (eq? 13 (value2 '(+ 1 (* 3 4)))))

(define 1st-sub-exp
  (lambda (exp)
    (car (cdr exp))))

                                        ; pg 106

(define 2nd-sub-exp
  (lambda (exp)
    (car (cdr (cdr exp)))))

(define operator
    (lambda (exp)
    (car exp)))

(define value
  (lambda (exp)
    (cond
     ((atom? exp) exp)
     ((eq? (operator exp) '+)
      (+ (value (1st-sub-exp exp))
         (value (2nd-sub-exp exp))))
     ((eq? (operator exp) '*)
      (* (value (1st-sub-exp exp))
         (value (2nd-sub-exp exp))))
     ((eq? (operator exp) '^)
      (** (value (1st-sub-exp exp))
          (value (2nd-sub-exp exp)))))))

(and (eq? 4 (value '(+ 1 3)))
     (eq? 13 (value '(+ 1 (* 3 4)))))

                                        ; pg 107

(define sero?
  (lambda (n)
    (null? n)))

(and (sero? '())
     (not (sero? 4)))

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
                                        ; pg 111

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(and (not (set? '(a b a c)))
     (set? '(a b c d))
     (set? '())
     (not (set? '(apple 3 pear 4 9 apple 3 4)))
     (set? '(apple 3 pear 4 9)))

                                        ; pg 112

(define makeset1
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset1 (cdr lat)))
     (else (cons (car lat) (makeset1 (cdr lat)))))))

(equal? '(c d a e b)
 (makeset1 '(a b c b d a e b)))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(equal? '(a b c d e)
 (makeset '(a b c b d a e b)))

                                        ; pg 113

(equal? '(a 3 p 4 9)
        (makeset '(a 3 p 4 9 a 3 4)))

(define subset1?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2)
      (subset1? (cdr set1) set2))
     (else #f))))

(and (subset1? '(5 c w) '(5 h 2 p f c a l d w))
     (not (subset1? '(4 p o h) '(4 p c a 5 oz h))))

                                        ; pg 114

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

(eqset? '(6 l c wi w) '(6 c wi l w))

                                        ; pg 115

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

(and (intersect? '(a b c d) '(d c e))
     (not (intersect? '(a b c) '(d e f)))
     (not (intersect? '() '(d c e)))
     (not (intersect? '(d c e) '())))

                                        ; pg 116

