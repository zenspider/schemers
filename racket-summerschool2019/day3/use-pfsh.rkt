#lang s-exp pfsh

(define (double s)
  (string-append s s))

(double "a a ")

(wc -w < (double "a a "))

(define out (ls "-1" -l))
out
42
(wc -l < out)
ls
