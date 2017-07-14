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
     b
     n
     s
     (nand e e)
     (+ e e)
     (- e e)
     (++ e e)
     )
  (b ::= boolean)
  (n ::= number)
  (s ::= string)
  )

;; TODO: not sure if I should be extending Lambda or PureLambda-E
;; what are the pros and cons?
(define-extended-language Lambda-E PureLambda-E
  (E ::= ....
     (nand E e)
     (nand v E)
     (+ E e)
     (+ v E)
     (- E e)
     (- v E)
     (++ E e)
     (++ v E)
     )
  (v ::=
     b
     n
     s
     )
  (b ::= boolean)
  (n ::= number)
  (s ::= string)
  )

(define Lambda->
  (extend-reduction-relation
   PureLambda-> Lambda-E
   (--> (in-hole E (nand b_1 b_2))
        (in-hole E ,(not (and (term b_1) (term b_2))))
        nand)
   (--> (in-hole E (+ v_1 v_2))
        (in-hole E ,(+ (term v_1) (term v_2)))
        plus)
   (--> (in-hole E (- v_1 v_2))
        (in-hole E ,(- (term v_1) (term v_2)))
        minus)
   (--> (in-hole E (++ s_1 s_2))
        (in-hole E ,(string-append (term s_1) (term s_2)))
        string-append))
  )

(module+ test
  (letrec ([x (lambda (i e) (test-->  Lambda-> i e))]
           [X (lambda (i e) (test-->> Lambda-> i e))])

    (define e04 (term (+ 1 2)))
    (define e05 (term (+ 1 (+ 2 3))))
    (define e06 (term (- 5 2)))
    (define e07 (term (- 3 (- 2 1))))
    (define e08 (term (++ "a" "b")))
    (define e09 (term (++ "a" (++ "b" "c"))))
    (define e10 (term (nand #t #t)))
    (define e11 (term (nand #f #f)))

    (x e04 (term 3))
    (X e05 (term 6))
    (x e06 (term 3))
    (X e07 (term 2))
    (x e08 (term "ab"))
    (x (term (++ "a" (+ 1 2))) (term (++ "a" 3))) ; TODO: failure
    (X e09 (term "abc"))
    (x e10 (term #f))
    (x e11 (term #t))
    ))

(default-language Lambda) ; Defines alpha-equivalence (eg test-equal)

;; (redex-match Lambda b (term #t))
;; (stepper Lambda-> (term (+ 1 (+ 2 3))))
;; (traces Lambda-> (term (+ 1 (+ 2 3))))

(module+ test
  (test-results))
