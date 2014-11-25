#lang racket/base

(require "../sicp/lib/test.rkt")
(require "ch04.rkt")                    ; eqan

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

