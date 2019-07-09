#lang racket

(require syntax/parse/define racket/stxparam
         (for-syntax racket/syntax))

(define-simple-macro (weirdo body)
  (let ([tmp 8]) body))

(let ([tmp 7])
  (+ tmp
     #;(let ([tmp 8]) tmp)
     (let ([let #f])
       (displayln let)
       (weirdo tmp))
     tmp))                              ; 21

;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (define-it val:expr)
  #:with external-it (datum->syntax #'val 'it)
  (define external-it val))

;; (define-it 5)
;; (+ it it)

(define-simple-macro (define-like base:id val:expr)
  #:with external-it (format-id #'base "~a-like" #'base)
  (define external-it val))

(define-like x 6)
(+ x-like x-like)

;;;;;;;;;;;;;;;;;;;;

(define-syntax-parameter it #f)

(define-simple-macro (with-it val:expr body:expr)
  (let ([this-it val])
    (syntax-parameterize ([it #;(λ (stx) #'this-it)
                              #;
                              (λ (stx)
                                (syntax-parse stx
                                  [x:id #'this-it]
                                  [(_ arg ...) #'(this-it arg ...)]))
                              (make-rename-transformer #'this-it)])
      body)))

(let ([it 7])
  (with-it 5
    (+ it it)))                        ; 14 -- bound to let, not macro

(with-it 5
  (+ it it))                            ; 10

;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (with-ignored e:expr ... (~literal EOF) _ ...)
  (let () e ...))

(define-syntax (EOF stx)
  (raise-syntax-error 'EOF "Cannot use outside of with-ignored*"
                      stx))

(define-syntax (with-ignored* stx)
  (syntax-parse stx
    #:literals (EOF)
    [(_ e:expr ... EOF ignored ...)     #'(begin e ...)]))

(with-ignored
  (define x 6)
  (+ x x)
  EOF
  (sleep 20))

(with-ignored*
  (define x 6)
  (+ x x)
  EOF
  (sleep 20))

;;;;;;;;;;;;;;;;;;;;

(define-syntax (method stx) (λ (stx) (raise-syntax-error #f "NOPE!" stx)))
(define-syntax (field  stx) (λ (stx) (raise-syntax-error #f "NOPE!" stx)))

(begin-for-syntax
  (define-splicing-syntax-class classy-clauses
    #:literals (field method)
    #:attributes ([fields        1]
                  [methods-names 1]
                  [methods-args  2]
                  [methods-body  2])

    (pattern (~seq)
             #:with
             (fields ...)
             #'()

             #:with
             (((methods-names methods-args ...) methods-body ...) ...)
             #'())

    (pattern (~seq (field f:id) rest:classy-clauses)
             #:with (fields ...) #'(f
                                    rest.fields
                                    ...)

             #:with
             (((methods-names       methods-args ...)    methods-body ...) ...)
             #'(; nothing
                ((rest.methods-names rest.methods-args ...) rest.methods-body ...)
                ...)
             )

    (pattern (~seq (method (name:id ma:id ...) mb:expr ...) rest:classy-clauses)
             #:with
             (fields ...)
             #'( ; nothing
                rest.fields
                ...)

             #:with
             (((methods-names       methods-args ...)    methods-body ...) ...)
             #'(((m ma ...) mb ...)
                ((rest.methods-names rest.methods-args ...) rest.methods-body ...)
                ...))

    ))

(define-simple-macro (classy name:id c:classy-clauses)
  42
  )

(classy
 posn
 (field x)
 (field y)
 (method (add-xs p2)
         (update #:x (+ x (posn.x p2)))))
