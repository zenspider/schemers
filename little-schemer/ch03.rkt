#lang racket/base

(provide multirember firsts)

(require "lib/shared.rkt")

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

