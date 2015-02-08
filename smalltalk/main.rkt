#!/usr/bin/env racket
#lang racket/base

(require (rename-in (prefix-in smalltalk/ "parser.rkt")))
(require "lexer.rkt")
(require "compiler.rkt")
(require racket/cmdline)
(require racket/pretty)
(require racket/function)               ; thunk

(define (smalltalk/parse+lex ip)
  (smalltalk/parse (thunk (smalltalk/lex ip))))

(define (smalltalk ip)
  (port-count-lines! ip)
  (pretty-print
   (smalltalk/compile (smalltalk/parse+lex ip))))

(module+ main
  (printf "parsing...~n")
  (let ([paths (command-line #:args paths paths)])
    (for ([path paths])
      (printf "~a~n" path)
      (smalltalk (open-input-file path)))))

(module+ test
  (require rackunit
           racket/sequence
           check-sexp-equal
           sexp-diff
           ragg/support                 ; token
           parser-tools/lex)            ; lexer-src-pos

  (define (test:smalltalk/parse+lex str)
    (syntax->datum (smalltalk/parse+lex (open-input-string str))))

  ;; (define-check (check-parse str expected)
  ;;   (with-handlers ([exn:fail:parsing?
  ;;                    (lambda (exn)
  ;;                      (fail-check (format "failed to parse: ~a" str)))])
  ;;     (let ([actual (test:smalltalk/parse+lex str)])
  ;;       (or (equal? actual expected)
  ;;           (fail-check (format "parse (#:new = actual, #:old = expected):~n~n~a"
  ;;                               (pretty-format (sexp-diff expected actual))))))))

  (define-check (check-parse str expected)
    (let ([actual (test:smalltalk/parse+lex str)])
      (or (equal? actual expected)
          (fail-check (format "parse (#:new = actual, #:old = expected):~n~n~a"
                              (pretty-format (sexp-diff expected actual)))))))

  (define (drain-lexer input)
    (define ip (open-input-string input))
    (sequence->list (in-producer smalltalk/lex eof ip)))

  (define (check-lex input . expecteds)
    (define actuals (drain-lexer input))
    (if (= (length actuals) (length expecteds))
        (for ([actual actuals] [expected expecteds])
          (check-sexp-equal? actual expected))
        (check-sexp-equal? actuals expecteds)))

  (define (tk t v m n)
    (position-token (token-struct t v #f #f #f #f #f)
                    (position m #f #f) (position n #f #f)))

  ;; eof, whitespace, & comments
  (check-lex "")
  (check-lex " ")
  (check-lex "\n")
  (check-lex "\t")
  (check-lex "\"comment\"")

  ;; numbers
  (check-lex "2"   (tk 'NUM "2" 1 2))
  (check-lex "2.3" (tk 'NUM "2.3" 1 4))
  ;; TODO: (check-lex "2r101" (tk 'NUM "2r101" 1 6))

  (check-parse "42"
               '(parse
                 (expression
                  (primary (literal (number "42"))))))

  ;; identifier
  (check-lex "abc" (tk 'IDENT "abc" 1 4))
  (check-lex "ab1" (tk 'IDENT "ab1" 1 4))

  (check-parse "abc"
               '(parse
                 (expression
                  (primary (variable-name (identifier "abc"))))))

  (define (check-symbol-const input sym)
    (check-parse input
                 `(parse
                   (expression
                    (primary (literal (symbol-const "#" ,sym)))))))

  ;; symbol const
  (check-symbol-const "#abc"  '(symbol (identifier "abc")))
  (check-symbol-const "#+"    '(symbol (binary-selector "+")))
  (check-symbol-const "#a:"   '(symbol (keyword (identifier "a") ":")))
  (check-symbol-const "#a:b:" '(symbol
                                (keyword (identifier "a") ":")
                                (keyword (identifier "b") ":")))

  ;; char
  (check-lex "$a"  (tk 'CHAR "$a" 1 3))
  (check-lex "$'"  (tk 'CHAR "$'" 1 3))
  (check-lex "$\"" (tk 'CHAR "$\"" 1 3))

  (check-parse "$a"
               '(parse
                 (expression
                  (primary (literal (character "$a"))))))

  ;; string
  (check-lex "'string'" (tk 'STR "'string'" 1 9))
  (check-lex "''"       (tk 'STR "''" 1 3))

  (check-parse "'abc'"
               '(parse (expression
                        (primary (literal (string "'abc'"))))))

  ;; array-const
  (check-parse "#( 1 )"
               '(parse
                 (expression
                  (primary
                   (literal
                    (array-const "#" (array "(" (number "1") ")")))))))

  ;; unary expressions
  (check-parse "1 msg"
               '(parse
                 (expression
                  (message-expression
                   (unary-expression
                    (unary-object (primary (literal (number "1"))))
                    (unary-selector (identifier "msg")))))))
  (check-parse "1 msgA msgB"
               '(parse
                 (expression
                  (message-expression
                   (unary-expression
                    (unary-object
                     (unary-expression
                      (unary-object (primary (literal (number "1"))))
                      (unary-selector (identifier "msgA"))))
                    (unary-selector (identifier "msgB")))))))

  (check-lex "2 + 3"
             (tk 'NUM        "2" 1 2)
             (tk 'BINARY_SEL "+" 3 4)
             (tk 'NUM        "3" 5 6))

  ;; binary expressions
  (check-parse "2 + 3"
               '(parse
                 (expression
                  (message-expression
                   (binary-expression
                    (binary-object (unary-object (primary (literal (number "2")))))
                    (binary-selector "+")
                    (unary-object (primary (literal (number "3")))))))))

  (check-parse "2 + 3 + 4"
               '(parse
                 (expression
                  (message-expression
                   (binary-expression
                    (binary-object
                     (binary-expression
                      (binary-object (unary-object (primary (literal (number "2")))))
                      (binary-selector "+")
                      (unary-object (primary (literal (number "3"))))))
                    (binary-selector "+")
                    (unary-object (primary (literal (number "4")))))))))

  ;; keyword expressions
  (check-parse "1 a: 2 b: 3"
               '(parse
                 (expression
                  (message-expression
                   (keyword-expression
                    (binary-object (unary-object (primary (literal (number "1")))))
                    (keyword (identifier "a") ":")
                    (binary-object (unary-object (primary (literal (number "2")))))
                    (keyword (identifier "b") ":")
                    (binary-object (unary-object (primary (literal (number "3"))))))))))

  ;; cascades
  (check-parse "1 a; b; c"
               '(parse
                 (expression
                  (cascade-expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (literal (number "1"))))
                     (unary-selector (identifier "a"))))
                   (cascade-extension
                    ";"
                    (unary-selector (identifier "b")))
                   (cascade-extension
                    ";"
                    (unary-selector (identifier "c")))))))

  (check-parse "1 a; + 3; c: 4"
               '(parse
                 (expression
                  (cascade-expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (literal (number "1"))))
                     (unary-selector (identifier "a"))))
                   (cascade-extension
                    ";"
                    (binary-selector "+")
                    (unary-object (primary (literal (number "3")))))
                   (cascade-extension
                    ";"
                    (keyword (identifier "c") ":")
                    (binary-object (unary-object (primary (literal (number "4"))))))))))

  ;; expressions
  (check-parse "a _ 42"                 ; assignment
               '(parse
                 (expression
                  (variable-name (identifier "a"))
                  "_"
                  (primary (literal (number "42"))))))
  (check-parse "a _ b _ 42"             ; multi-assignment
               '(parse
                 (expression
                  (variable-name (identifier "a"))
                  "_"
                  (variable-name (identifier "b"))
                  "_"
                  (primary (literal (number "42"))))))

  ;; statements

  (check-parse "a b."
               '(parse
                 (statements
                  (expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (variable-name (identifier "a"))))
                     (unary-selector (identifier "b")))))
                  ".")))

  (check-parse "a b. c d."
               '(parse
                 (statements
                  (expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (variable-name (identifier "a"))))
                     (unary-selector (identifier "b")))))
                  "."
                  (expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (variable-name (identifier "c"))))
                     (unary-selector (identifier "d")))))
                  ".")))

  (check-parse "a b. c d"
               '(parse
                 (statements
                  (expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (variable-name (identifier "a"))))
                     (unary-selector (identifier "b")))))
                  "."
                  (expression
                   (message-expression
                    (unary-expression
                     (unary-object (primary (variable-name (identifier "c"))))
                     (unary-selector (identifier "d"))))))))

  ;; (check-parse "a b. c d."
  ;;              42)
  ;; (check-parse "a b. c d. ^e"
  ;;              42)

  )
