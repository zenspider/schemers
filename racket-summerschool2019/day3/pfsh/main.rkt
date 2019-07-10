#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define
         "run.rkt")

(provide #%top-interaction
         string-append                  ; HACK
         #;(rename-out [pfsh:module-begin #%module-begin])
         #%module-begin
         (rename-out [pfsh:define define]
                     [pfsh:datum  #%datum]
                     [pfsh:top    #%top]
                     [pfsh:app    #%app]))

;; TODO: switch #%top-interaction to use display (?)

(module reader syntax/module-reader
  pfsh)

#;
(define-syntax (pfsh:module-begin stx)
  (syntax-parse stx
    [(_ (e ...) ...)
     #'(#%module-begin
        (void (run e ...)) ...)]))

(define-syntax (pfsh:app stx)
  (syntax-parse stx
    [(_ name:id arg ...)
     #:when (identifier-binding #'name)
     #'(#%app name arg ...)]
    [(_ other arg ...)
     #'(pfsh:run other arg ...)]))

(define-syntax (pfsh:datum stx)
  (syntax-parse stx
    [(_ . x:integer) #'(#%datum . x)]
    [(_ . x:str)     #'(#%datum . x)]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ k:id e:expr)
     #'(define k (with-output-to-string (lambda () e)))]
    [(_ (name:id arg:id ...) body ...)
     #'(define (name arg ...) body ...)]))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    #:datum-literals (<)
    [(_ prog arg ... < val)
     #'(run-with-input val prog arg ...)]
    [(_ prog arg ...)
     #'(run prog arg ...)]
    ))

(define-syntax (pfsh:top stx)
  (syntax-parse stx
    [(_ . sym:id)
     #`#,(symbol->string (syntax-e #'sym)) ]))
