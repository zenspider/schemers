#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define
         "run.rkt")

(provide #%top-interaction
         #;(rename-out [pfsh:module-begin #%module-begin])
         #%module-begin
         (rename-out [pfsh:define define]
                     [pfsh:datum  #%datum]
                     [pfsh:top    #%top]
                     [pfsh:run    #%app]))

;; TODO: switch #%top-interaction to use display (?)

#;
(define-syntax (pfsh:module-begin stx)
  (syntax-parse stx
    [(_ (e ...) ...)
     #'(#%module-begin
        (void (run e ...)) ...)]))

(define-syntax (pfsh:top stx)
  (syntax-parse stx
    [(_ . sym:id)
     #`#,(symbol->string (syntax-e #'sym)) ]))

(define-syntax (pfsh:datum stx)
  (syntax-parse stx
    [(_ . x) #'(#%datum . x)]))

(define-simple-macro (pfsh:define k:id e:expr)
  (define k (with-output-to-string (lambda () e))))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    #:datum-literals (<)
    [(_ prog arg ... < val:id)
     #'(run-with-input val prog arg ...)]
    [(_ prog arg ...)
     #'(run prog arg ...)]
    ))
