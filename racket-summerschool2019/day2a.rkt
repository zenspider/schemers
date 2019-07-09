#lang racket/base

(require (for-syntax racket/base
                     racket/contract
                     syntax/parse)
         racket/contract
         syntax/parse/define)

(define-simple-macro (simple-for/list/bad ([elem-name:id seq:expr])
                                      computation:expr)
  (map (λ (elem-name) computation) seq))

(simple-for/list/bad ([x (list 1 2 3)]) (add1 x))

(define-simple-macro (simple-for/list ([elem-name:id seq])
                                      computation:expr)
  #:declare seq (expr/c #'list?)
  (map (λ (elem-name) computation) seq.c))

#;(simple-for/list ([x 'not-a-list]) (add1 x))
(simple-for/list ([x (list 1 2 3)]) (add1 x))

(define-simple-macro (my-let ([x:id xe:expr] ...) body ...+)
  ((λ (x ...) body ...) xe ...))

(begin-for-syntax
  (define-syntax-class binding
    ;; #:attributes ([x 0] [xe 0])
    (pattern [x:id xe:expr])
    (pattern [xe:expr #:as x:id]))

  (define-splicing-syntax-class bindings
    ;; #:attributes ([x 1] [xe 1])
    ;; #:auto-nested-attributes
    (pattern (~seq b:binding ...)
             #:with (x  ...) #'(b.x  ...)
             #:with (xe ...) #'(b.xe ...)
             #:fail-when
             (check-duplicate-identifier (syntax->list #'(x ...)))
             "Duplicate ids"
             )))

(define-simple-macro (our-let (bs:bindings) body ...+)
  ((λ (bs.x ...) body ...) bs.xe ...))

(our-let ([x 3] [y 4])
         (+ x y))
(our-let ([3 #:as x] [y 4])
         (+ x y))

(define-simple-macro (where body bs:bindings)
  ((λ (bs.x ...) body) bs.xe ...))

((+ x y)
 . where .
 [x 3]
 [4 #:as y])
