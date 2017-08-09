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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VarLambda - extend Lambda with imperative features

(define-extended-language VarLambda Lambda
  (p ::= (prog (f ...) e))
  (f ::= (defun (x x) e))
  (e ::= ....
     ;; (let ([x e]) e)
     ;; (function x)
     )

  ;; #:binding-forms
  ;; (let ([x e_1]) e_2 #:refers-to x)
  )

(define-union-language HybridVarLambda VarLambda Lambda-E)

(define-extended-language VarLambda-E HybridVarLambda
  (P ::= (prog (f ...) E))
  (E ::= ....
     ;; (let ([x E]) e)
     )
  (v ::= ....
     (function x)
     )
  )

;; (default-language VarLambda) ; Defines alpha-equivalence (eg test-equal)

(define VarLambda->
  (extend-reduction-relation
   Lambda-> VarLambda-E
   #:domain p
   (--> (prog (f ...) (in-hole P v))
        v
        value)
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E x_fun))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E (function x_fun)))
        function-value)
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E (substitute e_body x_param v_arg)))
        function-apply)
   (--> (in-hole P ((λ (x) e_1) e_2))
        (in-hole P (substitute e_1 x e_2))
        apply)
   (--> (in-hole P (nand b_1 b_2))
        (in-hole P ,(not (and (term b_1) (term b_2))))
        nand)
   (--> (in-hole P (if #t e_1 e_2))
        (in-hole P e_1)
        if-t)
   (--> (in-hole P (if #f e_1 e_2))
        (in-hole P e_2)
        if-f)
   (--> (in-hole P (+ v_1 v_2))
        (in-hole P ,(+ (term v_1) (term v_2)))
        plus)
   (--> (in-hole P (- v_1 v_2))
        (in-hole P ,(- (term v_1) (term v_2)))
        minus)
   (--> (in-hole P (++ s_1 s_2))
        (in-hole P ,(string-append (term s_1) (term s_2)))
        string-append)
   ;; (--> (in-hole P (let ([x v]) e))
   ;;      (in-hole P (substitute e x v))  ; TODO: or lambda application?
   ;;      let)
   ))

(module+ test
  (parameterize ([default-language VarLambda])
   (letrec ((wrap (lambda (x) (term (prog () ,x))))
            [x (lambda (i e) (test-->  VarLambda-> (wrap i) (wrap e)))]
            [X (lambda (i e) (test-->> VarLambda-> (wrap i) (wrap e)))])
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

;; (begin
;;   (require redex)
;;   (extend-language-show-union #t)
;;   (render-language VarLambda)
;;   (render-reduction-relation VarLambda->)
;;   (traces VarLambda-> e13))

;; (define t (term (if #t 1 2)))
;; (redex-match Lambda e t)
;; (test--> Lambda-> t 1)
;; (apply-reduction-relation* Lambda-> t)
;; (require redex)
;; (stepper Lambda-> t)
;; (traces Lambda-> t)

(module+ test
  (test-results))
