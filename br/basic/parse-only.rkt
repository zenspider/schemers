#lang br/quicklang

(provide (rename-out [parser-only-mb #%module-begin]))

(module+ reader
  (provide read-syntax))

(require "parser.rkt" "tokenizer.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings #`(module basic-parser-mod basic/parse-only #,parse-tree)))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin 'PARSE-TREE))
