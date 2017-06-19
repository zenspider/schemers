#lang br/quicklang

(module+ reader
  (provide read-syntax))

(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings #`(module basic-mod basic/expander
                      #,parse-tree)))

(module+ test
  (require rackunit))

(module+ test
  ;; TODO: Tests to be run with raco test
  )
