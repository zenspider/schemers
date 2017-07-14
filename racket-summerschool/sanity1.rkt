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

;;; Lambda - extend PureLambda with simple values and operations

(define-extended-language Lambda PureLambda
  (e ::= ....
     n
     (+ e e)
     (- e e)
     )
  (n ::= number))

(default-language Lambda)

(define-extended-language Lambda-E PureLambda-E
  (E ::= ....
     (+ E e)
     (+ v E)
     (- E e)
     (- v E)
     )
  (v ::=
     n)
  (n ::= number))

(define Lambda->
  (extend-reduction-relation
   PureLambda-> Lambda-E
   (--> (in-hole E (+ v_1 v_2))
        (in-hole E ,(+ (term v_1) (term v_2)))
        plus)
   (--> (in-hole E (- v_1 v_2))
        (in-hole E ,(- (term v_1) (term v_2)))
        minus))
  )

(module+ test
  (letrec ([x (lambda (i e) (test-->  Lambda-> i e))]
           [X (lambda (i e) (test-->> Lambda-> i e))])

    (define e04 (term (+ 1 2)))
    (define e05 (term (+ 1 (+ 2 3))))
    (define e06 (term (- 5 2)))
    (define e07 (term (- 3 (- 2 1))))

    (x e04 (term 3))
    (X e05 (term 6))
    (x e06 (term 3))
    (X e07 (term 2))
    ))

;; (stepper Lambda-> (term (+ 1 (+ 2 3))))
;; (traces Lambda-> (term (+ 1 (+ 2 3))))

(module+ test
  (test-results))
