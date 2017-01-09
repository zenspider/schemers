#lang br/quicklang

(provide + * (rename-out [stackerizer-mb #%module-begin]))

(define-macro (stackerizer-mb EXPR ...)
  #'(#%module-begin
     (begin (printf ";; ~a~n" EXPR)
            (for-each displayln (reverse (flatten EXPR)))
            (newline)) ...))

(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...)) #'(list 'OP FIRST (OP NEXT (... ...)))])
      ...))

(define-ops * +)
