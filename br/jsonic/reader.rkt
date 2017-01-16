#lang br/quicklang

(provide (contract-out [read-syntax (any/c input-port? . -> . syntax?)]))

(require "tokenizer.rkt"
         "parser.rkt")

(define (read-syntax path port)
  (define datum `(module jsonic-module jsonic/expander
                   ,(parse path (tokenize port))))
  (datum->syntax #f datum))

(module+ test
  (require rackunit)

  (check-equal? (syntax->datum (read-syntax #f (open-input-string "//x\ny\nz")))
                '(module jsonic-module jsonic/expander
                   (jsonic-program (json-char "y")
                                   (json-char "\n")
                                   (json-char "z"))))

  (displayln 'done))
