#!/usr/local/bin/csi -s

(use test)

(define is-first-b?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) a)
                    (two-in-a-row? lat))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else
           (is-first-b? (car lat) (cdr lat))))))

(test-group "two-in-a-row?"
 (test #f (two-in-a-row? '(a b c)))
 (test #f (two-in-a-row? '(a b a)))
 (test #t (two-in-a-row? '(a b b))))

