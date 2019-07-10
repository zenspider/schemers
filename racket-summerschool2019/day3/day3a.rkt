#lang racket

(require "pfsh-run.rkt"
         "five.rkt"
         syntax/parse/define)

(run ls -l)

(define-five x (+ 41 1))

x
