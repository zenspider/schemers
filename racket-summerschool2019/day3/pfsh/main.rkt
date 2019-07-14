#lang macracket

(require "run.rkt")

(provide #%top-interaction
         string-append                  ; HACK
         < >
         #%module-begin
         (rename-out [pfsh:define define]
                     [pfsh:datum  #%datum]
                     [pfsh:top    #%top]
                     [pfsh:app    #%app]))

;; TODO: switch #%top-interaction to use display (?)

(module reader syntax/module-reader
  pfsh

  #:wrapper1 pfsh:wrapper

  (require racket/list)

  (define (gather stxs)
    (define (alone? e) (and (= 1 (length e)) (pair? (syntax-e (car e)))))
    (define (wrap e)   (if (alone? e) e (list e)))
    (append-map wrap (group-by syntax-line stxs =)))

  (define (pfsh:wrapper t)
    (gather (t))))

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
