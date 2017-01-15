#lang br/quicklang

(provide tokenize)

(require brag/support)

(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(from/to "//" "\n") (next-token)]  ; skip comments
       [(from/to "@$" "$@") (token 'SEXP-TOK (trim-ends "@$" lexeme "$@"))]
       [any-char (token 'CHAR-TOK lexeme)]
       [(eof) eof]                      ; eof
       ))
    (our-lexer port))
  next-token)

(module+ test
  (apply-tokenizer tokenize "// comment\n")
  (apply-tokenizer tokenize "@$ (+ 6 7) $@")
  (apply-tokenizer tokenize "hi"))
