#lang br/quicklang

(provide (rename-out [tokenize-only-mb #%module-begin]))

(module+ reader
  (provide read-syntax))

(require brag/support "tokenizer.rkt")

(define (read-syntax path port)
  (define tokens (apply-tokenizer make-tokenizer port))
  (strip-bindings #`(module basic-tokens-mod basic/tokenize-only #,@tokens)))

(define-macro (tokenize-only-mb TOKEN ...)
  #'(#%module-begin (list TOKEN ...)))
