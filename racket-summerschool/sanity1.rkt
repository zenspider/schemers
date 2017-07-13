#lang racket

(require redex)

;;; PureLambda - nothing extra at all

(define-language PureLambda
  (e ::=
     x
     (λ (x) e)
     (e e))

  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (λ (x) e #:refers-to x))

(default-language PureLambda) ; Defines alpha-equivalence (eg test-equal)

(define-extended-language PureLambda-E PureLambda
  (E ::=
     hole
     (λ (x) E)
     (E e)
     (e E)))

(define PureLambda->
  (reduction-relation
    PureLambda-E
    #:domain e
    (--> (in-hole E ((λ (x) e_1) e_2))
         (in-hole E (substitute e_1 x e_2))
         apply)))

(module+ test
  (letrec ([x️ (lambda (i e) (test-->  PureLambda-> i e))]
           [X️ (lambda (i e) (test-->> PureLambda-> i e))])

    (define e01 (term ((λ (x) x) x)))
    (define e02 (term ((λ (x) x)
                       (λ (x) x))))
    (define e03 (term ((λ (x) (λ (y) x))
                       ((λ (x) x) z))))

    (x️ e01 (term x))
    (x️ e02 (term (λ (x) x)))
    (X️ e03 (term (λ (y) z)))))

(module+ test
  (test-results))
