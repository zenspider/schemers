#lang br

(provide basic-lexer)

(require brag/support)

(define-lex-abbrev digits (:+ numeric))

(define-lex-abbrev reserved-terms
  (:or "print" "goto" "end" "+" ":" ";" "let" "=" "input"
       "-" "*" "/" "^" "mod" "(" ")"
       "if" "then" "else" "<" ">" "<>" "and" "or" "not"
       "gosub" "return" "for" "to" "step" "next"
       "def" ","
       "import"
       "export"
       ))

(define-lex-abbrev racket-id
  (:~ (:or whitespace (char-set "()[]{}\",'`;#|\\"))))

(define basic-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ["\n"  (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "rem" "\n") (token 'REM lexeme)]
   [(:seq "[" (:+ racket-id) "]") (token 'RACKET-ID
                                         (string->symbol
                                          (trim-ends "[" lexeme "]")))]
   [reserved-terms (token lexeme lexeme)]
   [(:seq alphabetic (:* (:or alphabetic numeric "$")))
    (token 'ID (string->symbol lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:? digits) "." digits)
         (:seq digits "."))
    (token 'DECIMAL (string->number lexeme))]
   [(:or (from/to "\"" "\"")
         (from/to "'"  "'"))
    (token 'STRING (string-trim lexeme #px"."))]))

(module+ test
  (require rackunit
           racket/sequence
           brag/support)

  (define (lex str)
    (apply-lexer basic-lexer str))

  (define-syntax-rule (check-lex input results ...)
    (check-equal? (lex input)
                  (for/list ([pair (in-slice 2 (list results ...))])
                    (apply srcloc-token pair))))
  (define (loc from to)
    (srcloc 'string #f #f from to))
  (define (check-token word)
    (check-lex word
               (token word word)
               (loc 1 (string-length word))))

  (check-lex "")
  (check-lex " "
             (token " " #:skip? #t)
             (loc 1 1))
  (check-lex "rem ignored\n"
             (token 'REM "rem ignored")
             (loc 1 11)
             (token 'NEWLINE "\n")
             (loc 12 1))
  (check-token "print")
  (check-token "goto")
  (check-token "end")
  (check-token "+")
  (check-token ";")
  (check-token ":")
  (check-lex "12"
             (token 'INTEGER 12)
             (loc 1 2))
  (check-lex "1.2"
             (token 'DECIMAL 1.2)
             (loc 1 3))
  (check-lex "12."
             (token 'DECIMAL 12.0)
             (loc 1 3))
  (check-lex ".12"
             (token 'DECIMAL 0.12)
             (loc 1 3))
  (check-lex "\"foo\""
             (token 'STRING "foo")
             (loc 1 5))
  (check-lex "'foo'"
             (token 'STRING "foo")
             (loc 1 5))

  (printf "done~n"))
