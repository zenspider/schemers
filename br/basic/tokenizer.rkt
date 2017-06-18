#lang br

(provide make-tokenizer)

(require brag/support
         "lexer.rkt")

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (lexer-file-path path)
  (define (next-token) (basic-lexer ip))
  next-token)
