#lang br/quicklang

(require "struct.rkt" "run.rkt" "productions.rkt")

(provide (rename-out [b-module-begin #%module-begin])
         (all-from-out "productions.rkt"))

(define-macro (b-module-begin (b-program LINE ...))  ; TODO: rename b-program?
  (with-pattern
    ([((b-line NUM STATEMENT ...) ...) #'(LINE ...)]
     [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))])
    #'(#%module-begin
       LINE ...
       (define line-table (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (void (run line-table)))))
