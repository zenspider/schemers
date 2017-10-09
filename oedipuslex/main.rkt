#lang racket
;; TODO: Use racket/base but generated modules blow up on string-trim
;;       despite having requires for racket/string.

(require (for-syntax racket/base racket/list racket/sequence)
         br-parser-tools/lex
         brag/support
         syntax/parse/define)

(provide (rename-out (oedipus-mb #%module-begin))
         (except-out (all-from-out racket) #%module-begin)
         (all-from-out br-parser-tools/lex brag/support))

(module reader syntax/module-reader     ; pass to our own expander
  oedipuslex)

(define-for-syntax (filter-triplets match stxs)
  ; '((a b c) ...) -> ((a c) ...) when b=match
  (for/list ([ex (in-slice 3 (syntax->list stxs))]
             #:when (equal? (syntax-e (second ex)) match))
    (list (first ex) (third ex))))

(define-syntax (oedipus-mb stx)
  (syntax-parse stx
    [(_ . rest)
     #`(#%module-begin
        (provide lex)
        (define-lex-abbrevs #,@(filter-triplets '= #'rest))
        (define lex (lexer-srcloc #,@(filter-triplets ': #'rest))))]))
