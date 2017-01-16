#lang br/quicklang

(provide (contract-out [tokenize (input-port? . -> . (-> token?))]))

(require brag/support)

(module+ test
  (require rackunit))

(define (token? x)
  (or (eof-object? x) (string? x) (token-struct? x)))

(module+ test
  (check-true (token? eof))
  (check-true (token? "a string"))
  (check-true (token? (token 'A-TOKEN-STRUCT "hi")))
  (check-false (token? 42)))

(define (tokenize port)
  (port-count-lines! port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(from/to "//" "\n") (next-token)]  ; skip comments
       [(from/to "@$" "$@") (token 'SEXP-TOK (trim-ends "@$" lexeme "$@")
                                   #:position (+ (pos lexeme-start) 2)
                                   #:line     (line lexeme-start)
                                   #:column   (+ (col lexeme-start) 2)
                                   #:span     (- (pos lexeme-end)
                                                 (pos lexeme-start) 4))]

       [any-char (token 'CHAR-TOK lexeme
                        #:position (pos lexeme-start)
                        #:line     (line lexeme-start)
                        #:column   (col lexeme-start)
                        #:span     (- (pos lexeme-end)
                                      (pos lexeme-start)))]
       [(eof) eof]                      ; eof
       ))
    (our-lexer port))
  next-token)

(module+ test
  (check-equal? (apply-tokenizer tokenize "// comment\n")
                empty)
  (check-equal? (apply-tokenizer tokenize "@$ (+ 6 7) $@")
                (list (token-struct 'SEXP-TOK " (+ 6 7) " 3 1 2 9 #f)))
  (check-equal? (apply-tokenizer tokenize "hi")
                (list
                 (token-struct 'CHAR-TOK "h" 1 1 0 1 #f)
                 (token-struct 'CHAR-TOK "i" 2 1 1 1 #f)))
  (displayln 'done))
