(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.62:

;; Give a [theta](n) implementation of `union-set' for sets
;; represented as ordered lists.

(define (union-set-ugly s1 s2)
  (cond ((null? s2) s1)
        ((null? s1) s2)
        ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
        (else                  (cons (car s2) (union-set s1 (cdr s2))))))

(define (union-set s1 s2)
  (cond ((null? s2) s1)
        ((null? s1) s2)
        (else
         (let ((x1 (car s1)) (x2 (car s2))
               (r1 (cdr s1)) (r2 (cdr s2)))
           (cond
            ((= x1 x2) (cons x1 (union-set r1 r2)))
            ((< x1 x2) (cons x1 (union-set r1 s2)))
            (else      (cons x2 (union-set s1 r2))))))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(assert-many (lambda (f)
               (assert-equal '(2 3 4) (f '(2 4) '(2 3)))
               (assert-equal '(1 2 3) (f '(1) '(2 3)))
               (assert-equal '(1 2 3) (f '(2) '(1 3)))
               (assert-equal '(1 2 3) (f '(3) '(1 2))))
             union-set-ugly
             union-set)

(done)

(done)
