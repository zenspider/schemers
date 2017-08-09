#lang racket

(require redex/reduction-semantics)
(check-redundancy #t)

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
  (define e01 (term ((λ (x) x) x)))
  (define e02 (term ((λ (x) x)
                     (λ (x) x))))
  (define e03 (term ((λ (x) (λ (y) x))
                     ((λ (x) x) z))))

  (parameterize ([default-language PureLambda])
   (letrec ([x (lambda (i e) (test-->  PureLambda-> i e))]
            [X (lambda (i e) (test-->> PureLambda-> i e))])

     (x e01 (term x))
     (x e02 (term (λ (x) x)))
     (X e03 (term (λ (y) z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lambda - extend PureLambda with simple values and operations

(define-extended-language Lambda PureLambda
  (e ::= ....
     b
     n
     s
     (nand e e)
     (if e e e)
     (+ e e)
     (- e e)
     (++ e e)
     )
  (b ::= boolean)
  (n ::= number)
  (s ::= string)
  )

(define-union-language HybridLambda Lambda PureLambda-E)

(define-extended-language Lambda-E HybridLambda
  (E ::= ....
     (nand E e)
     (nand v E)
     (if E e e)
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
  )

(define Lambda->
  (extend-reduction-relation
   PureLambda-> Lambda-E
   (--> (in-hole E (nand b_1 b_2))
        (in-hole E ,(not (and (term b_1) (term b_2))))
        nand)
   (--> (in-hole E (if #t e_1 e_2))
        (in-hole E e_1)
        if-t)
   (--> (in-hole E (if #f e_1 e_2))
        (in-hole E e_2)
        if-f)
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
  (define e04 (term (+ 1 2)))
  (define e05 (term (+ 1 (+ 2 3))))
  (define e06 (term (- 5 2)))
  (define e07 (term (- 3 (- 2 1))))
  (define e08 (term (++ "a" "b")))
  (define e09 (term (++ "a" (++ "b" "c"))))
  (define e10 (term (nand #t #t)))
  (define e11 (term (nand #f #f)))
  (define e12 (term (if #t 1 2)))
  (define e13 (term (if #f 1 2)))

  (parameterize ([default-language Lambda])
   (letrec ([x (lambda (i e) (test-->  Lambda-> i e))]
            [X (lambda (i e) (test-->> Lambda-> i e))])

     (x e01 (term x))                   ; previous tests
     (x e02 (term (λ (x) x)))
     (X e03 (term (λ (y) z)))

     (x e04 (term 3))
     (X e05 (term 6))
     (x e06 (term 3))
     (X e07 (term 2))
     (x e08 (term "ab"))
     (x (term (++ "a" (+ 1 2))) (term (++ "a" 3))) ; TODO: failure
     (X e09 (term "abc"))
     (x e10 (term #f))
     (x e11 (term #t))
     (x e12 (term 1))
     (x e13 (term 2))
     )))

;; (define t (term (if #t 1 2)))
;; (redex-match Lambda e t)
;; (test--> Lambda-> t 1)
;; (apply-reduction-relation* Lambda-> t)
;; (require redex)
;; (stepper Lambda-> t)
;; (traces Lambda-> t)

(module+ test
  (test-results))
