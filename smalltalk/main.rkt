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
  (require rackunit)

  (define (test:smalltalk/parse+lex str)
    (syntax->datum (smalltalk/parse+lex (open-input-string str))))

  (define starter-string "Object comment: 'a comment'. !")

  (check-equal? (test:smalltalk/parse+lex starter-string)
                '(parse
                  (classDefinition
                   (staticStatement
                    (className (identifier "Object"))
                    (keyword "comment:")
                    (string "'a comment'")
                    ".")) "!"))

  (check-equal? (smalltalk (open-input-string starter-string))
                42)

  )
