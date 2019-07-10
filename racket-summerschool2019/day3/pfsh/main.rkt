#lang racket

(require (for-syntax syntax/parse racket/list)
         racket/function
         syntax/parse/define
         "run.rkt")

(provide #%top-interaction
         string-append                  ; HACK
         < >
         (rename-out [pfsh:define define]
                     [pfsh:module-begin #%module-begin]
                     [pfsh:datum  #%datum]
                     [pfsh:top    #%top]
                     [pfsh:app    #%app]))

;; TODO: switch #%top-interaction to use display (?)

(module reader syntax/module-reader
  pfsh)

(define-syntax (pfsh:module-begin stx)
  (syntax-parse stx
    [(_ e ...)
     (define (stand-alone e) (and (= 1 (length e)) (pair? (syntax-e (car e)))))
     (define (wrap e)        (if (stand-alone e) e (list e)))
     (define per-line      (group-by syntax-line (syntax->list #'(e ...)) =))
     (define wrapped-exprs (datum->syntax stx (append-map wrap per-line)))
     #`(#%module-begin
        #,@wrapped-exprs
        )]))

(define-syntax (< stx)
  (raise-syntax-error '< "Cannot use redirection outside of run") stx)

(define-syntax (> stx)
  (raise-syntax-error '> "Cannot use redirection outside of run") stx)

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
     #'(define k e)]
    [(_ (name:id arg:id ...) body ...)
     #'(define (name arg ...) body ...)]))

(define-syntax (pfsh:run stx)
  (syntax-parse stx
    #:literals (>)
    [(_ prog arg ... > out)
     #'(define out (with-output-to-string (thunk (pfsh:run* prog arg ...))))]
    [(_ prog arg ...)
     #'(void (pfsh:run* prog arg ...))]
    ))

(define-syntax (pfsh:run* stx)
  (syntax-parse stx
    #:literals (<)
    [(_ prog arg ... < in)
     #'(run-with-input in prog arg ...)]
    [(_ prog arg ...)
     #'(run prog arg ...)]
    ))

(define-syntax (pfsh:top stx)
  (syntax-parse stx
    [(_ . sym:id)
     #`#,(symbol->string (syntax-e #'sym)) ]))
