#lang br/quicklang

(require "parser.rkt")
(require parser-tools/lex)

(provide read-syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module bf-mod bf/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))

(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer [(eof) eof]
             [(char-set "><-.,+[]") lexeme]
             [any-char (next-token)]))
    (our-lexer port))
  next-token)

(module+ test
  (require rackunit)

  (check-equal? (test-reader read-syntax "++-[>-<].")
                '(module bf-mod bf/expander
                   (bf-program (op "+")
                               (op "+")
                               (op "-")
                               (loop "["
                                     (op ">")
                                     (op "-")
                                     (op "<")
                                     "]")
                               (op "."))))
  (displayln 'done)
  )
