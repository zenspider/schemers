#lang pfsh

(define (double s)
  (string-append s s))

(double "a a ")

(wc -w < (double "a a "))

(ls "-1" -l > out)
out
42
(wc -l < out)
ls

(define you (double "YOU"))

(whoami > me)
(echo Hello me)
(echo Hello you)
