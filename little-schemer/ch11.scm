#!/usr/local/bin/csi -s

(use test)

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond ((null? lat) #f)
          (else (or (eq? (car lat) preceding)
                    (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else
           (two-in-a-row-b? (car lat) (cdr lat))))))

(test-group "two-in-a-row?"
 (test #f (two-in-a-row? '(a b c d)))
 (test #f (two-in-a-row? '(a b a d)))
 (test #t (two-in-a-row? '(a b b d)))
 (test #t (two-in-a-row? '(a a c d)))
 (test #t (two-in-a-row? '(a b c c))))

