#lang racket

(require "run.rkt"
         (for-syntax syntax/parse))

(provide (rename-out [pfsh:run run]))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ...)
     #'(run (symbol->string 'prog) (symbol->string 'arg) ...)]))
