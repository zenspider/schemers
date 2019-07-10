#lang pfsh

(define (double s)
  (string-append s s))

(double "a a ")

(wc -w < (double "a a ") > woot)

woot

(ls "-1" -l > out)
out
42
(wc -l < out)
(wc -l < out > xxx)
(echo HAHAHA ": " xxx)
(wc -l < out > boo)
(echo HAHAHA ": " boo)
ls

(define you (double "YOU"))

(whoami > me)
(echo Hello me)
(echo Hello you)
