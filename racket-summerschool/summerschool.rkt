#lang racket

(require redex)

(define-language Λ
  (e ::=
     x
     (λ (x) e)
     (e e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (x) e #:refers-to x))

(define term1 (term (λ (x) y)))

term1

;; 923 Λ

(default-language Λ)

(term (substitute ,term1 y x))

(define-extended-language Lambda-calculus Λ
  (C ::=
     hole
     (λ (x) C)
     (C e)
     (e C)))

;; hole and in-hole are built into redex & have meaning

(define ->name                          ; q
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole C ((λ (x) e_1) e_2))
        (in-hole C (substitute e_1 x e_2))
        beta-name)))

;; x & e are meta-variables, y is just a concrete y in the grammar

(define ->value                         ; r
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole C ((λ (x) e_1) (λ (x_y) e_2))) ; x_y to avoid y in grammar
        (in-hole C (substitute e_1 x e_2))
        beta-value)))

(apply-reduction-relation ->name (term ((λ (x) y) x))) ; '(y)
(apply-reduction-relation ->value (term ((λ (x) y) x))) ; '() no term to relate
;; (apply-reduction-relation* ->name (term ((λ (x) ((λ (z) z) a)) x)))

(traces ->name (term ((λ (x) ((λ (z) z) a)) x)))
(traces ->value (term ((λ (x) ((λ (z) z) a)) x)))
;; (traces ->name (term ((λ (x) (x x)) (λ (x) (x x)) )))

;; we've caught up to 1970!

;; TIP for writing papers or modeling in redex everything that has to
;; do with formulating the semantics, goes in a separate section

;; write down the grammar separately

;; write down a separate section w/ semantics

:; ------------------------------------------------------------

(define-language PCF
  (e ::=
     x
     (λ (x) e)
     (e e)

     tt
     ff
     (if e e e)

     n
     (e + e))

  (n ::=
     integer))

(define-extended-language PCF-eval PCF  ; this bakes in the LMOM strategy
  (E-name ::=
          hole
          (E-name e)
          (E-name + e)
          (if E-name e e)
          (v + E-name))
  (v ::=
     n
     tt
     ff
     (lambda (x) e)))

(define ->name
  (reduction-relation
   PCF-eval
   #:domain e
   (--> (in-hole E-name ((λ (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)
   (--> (in-hole E-name (if tt e_1 e_2))
        (in-hole E-name e_1)
        if-tt)
   (--> (in-hole E-name (if ff e_1 e_2))
        (in-hole E-name e_2)
        if-ff)
   (--> (in-hole E-name (n_1 + n_2))
        (in-hole E-name ,(+ (term n_1) (term n_2)))
        plus)))

(traces (term ))
