#lang br/quicklang

(provide (contract-out [tokenize (input-port? . -> . (-> token?))]))

(require brag/support)

(define (token? x)
  (or (eof-object? x) (string? x) (token-struct? x)))

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
