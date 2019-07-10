#lang racket

(require "pfsh-run.rkt"
         "five.rkt"
         syntax/parse/define)

(run ls -l)

(define x (+ 41 1))
(define y 314)

(+ x y)
