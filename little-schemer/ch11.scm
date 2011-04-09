#!/usr/local/bin/csi -s

(use test)

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else
           (or (is-first? (car lat) (cdr lat))
               (two-in-a-row? (cdr lat)))))))

(test-group "two-in-a-row?"
 (test #f (two-in-a-row? '(a b c)))
 (test #f (two-in-a-row? '(a b a)))
 (test #t (two-in-a-row? '(a b b))))

