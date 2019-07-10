#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define
         "run.rkt")

(provide #%top-interaction
         #;(rename-out [pfsh-module-begin #%module-begin])
         #%module-begin
         (rename-out [pfsh:define define]
                     [pfsh:run    run]))

;; TODO: switch #%top-interaction to use display (?)

#;
(define-syntax (pfsh-module-begin stx)
  (syntax-parse stx
    [(_ (e ...) ...)
     #'(#%module-begin
        (void (run e ...)) ...)]))

(define-simple-macro (pfsh:define k:id e:expr)
  (define k (with-output-to-string (lambda () e))))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ...)
     #'(run (symbol->string 'prog) (symbol->string 'arg) ...)]
    ))
