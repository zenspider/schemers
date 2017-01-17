#lang info

(define collection "jsonic")
(define version "1.0")
(define scribblings '(("scribblings/jsonic.scrbl")))
(define test-omit-paths '("jsonic-demo.rkt" "jsonic-demo2.rkt"))
(define deps '("base"
               "beautiful-racket-lib"
               "brag"
               "draw-lib"
               "gui-lib"
               "parser-tools-lib"
               "syntax-color-lib"))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "scribble-lib"))
