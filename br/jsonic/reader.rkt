#lang br/quicklang

(provide read-syntax)

(require "tokenizer.rkt"
         "parser.rkt")

(define (read-syntax path port)
  (define datum `(module jsonic-module jsonic/expander
                   ,(parse path (tokenize port))))
  (datum->syntax #f datum))
