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

  (define (test-purelambda reduction [wrap identity])
    (test-->  reduction (wrap e01) (wrap (term x)))
    (test-->  reduction (wrap e02) (wrap (term (λ (x) x))))
    (test-->> reduction (wrap e03) (wrap (term (λ (y) z)))))

  (parameterize ([default-language PureLambda])
    (test-purelambda PureLambda->)))

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
  (define e09/e (term (++ "a" (+ 1 2))))
  (define e10 (term (nand #t #t)))
  (define e11 (term (nand #f #f)))
  (define e12 (term (if #t 1 2)))
  (define e13 (term (if #f 1 2)))

  (define (test-lambda reduction [wrap identity])
    (test-->  reduction (wrap e04)   (wrap (term 3)))
    (test-->> reduction (wrap e05)   (wrap (term 6)))
    (test-->  reduction (wrap e06)   (wrap (term 3)))
    (test-->> reduction (wrap e07)   (wrap (term 2)))
    (test-->  reduction (wrap e08)   (wrap (term "ab")))
    (test-->  reduction (wrap e09/e) (wrap (term (++ "a" 3))))
    ;; (test-->> reduction (wrap e09/e) (wrap (term (err "blah"))))
    (test-->> reduction (wrap e09)   (wrap (term "abc")))
    (test-->  reduction (wrap e10)   (wrap (term #f)))
    (test-->  reduction (wrap e11)   (wrap (term #t)))
    (test-->  reduction (wrap e12)   (wrap (term 1)))
    (test-->  reduction (wrap e13)   (wrap (term 2))))

  (parameterize ([default-language Lambda])
    (test-purelambda Lambda->)          ; previous tests
    (test-lambda Lambda->)))

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
    (letrec ([wrap (lambda (x) (term (prog () ,x)))])
      (test-purelambda VarLambda-> wrap)          ; previous tests
      (test-lambda VarLambda-> wrap)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PCF - starting over via the doco... I don't understand SOMETHING

(define-language PCF
  (p ::=
     (prog (f ...) e))

  (f ::=
     (defvar x v))

  (v ::=
     n
     tt
     ff
     (λ (x) e))

  (e ::=
     (set! x e)                         ; NOTE: not a binding construct

     x
     (λ (x) e)
     (e e)

     ; booleans
     tt
     ff
     (if e e e)

     ; arithmetic
     n
     (e + e))

  (n ::=
     integer)

  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (λ (x) e #:refers-to x))

(define-metafunction PCF
  let : ((x e)) e e -> e
  [(let ([x_lhs e_rhs]) e_1 e_2)
   ((λ (x_lhs)
      ((λ (x_dummy) e_2) e_1))
    e_rhs)
   (where (x_dummy) ,(variables-not-in (term (e_1 e_2)) '(dummy)))])

(define-extended-language PCF-eval PCF
  (P ::=
     (prog (f ...) E))
  (E ::=
     hole
     (set! x E)
     (if E e e)
     (E e)
     (v E)
     (E + e)
     (v + E)))

(define ->value
  (reduction-relation PCF-eval #:domain p
    (--> (in-hole P (if tt e_1 e_2))
         (in-hole P e_1)
         if-tt)
    (--> (in-hole P (if ff e_1 e_2))
         (in-hole P e_2)
         if-ff)
    (--> (in-hole P (n_1 + n_2))
         (in-hole P ,(+ (term n_1) (term n_2)))
         plus)
    (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
               (in-hole E x))
         (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
               (in-hole E v))
         retrieve)
    (--> (prog ((defvar x_1 v_1) ... (defvar x v)     (defvar x_2 v_2) ...)
               (in-hole E (set! x v_new)))
         (prog ((defvar x_1 v_1) ... (defvar x v_new) (defvar x_2 v_2) ...)
               (in-hole E 81))
         assignment)
    (--> (prog ((defvar x_1 v_1) ...)              (in-hole E ((λ (x) e) v)))
         (prog ((defvar x_1 v_1) ... (defvar x v)) (in-hole E e))
         allocation)))

(define-metafunction PCF-eval
  eval : p -> v or (err any)
  [(eval p)                             ; is this crazy?
   (λ (x_3) v)
   (where ((prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
                 (λ (x_3) x)))
          ,(apply-reduction-relation* ->value (term p)))]
  [(eval p)
   v
   (where ((prog (f ...) v))
          ,(apply-reduction-relation* ->value (term p)))]
  [(eval p)
   (err ,(apply-reduction-relation* ->value (term p)))])

(module+ test
  (parameterize ([default-language PCF])
    (define e1
      (term
       (prog ()
             (((λ (x)
                 (λ (y)
                   (let ([tmp x])
                     (set! x (y + 1))
                     tmp)))
               1)
              2))))

    (define e2
      (term
       (prog ()
             ((λ (y)
                ((λ (z)
                   ((λ (x)
                      (let ([tmp x])
                        (set! x y)
                        tmp))
                    (let ([tmp-z z])
                      (set! z (z + 1))
                      (let ([tmp-y y])
                        (set! y tmp-z)
                        tmp-y))))
                 1))
              2))))

    (define e3
      (term
       (prog ()
             ((λ (x) (λ (y) x))
              ((λ (x) x) 2)))))

    (define e4
      (term
       (prog ()
             (((λ (x) (λ (y) x))
               (1 + (1 + 1)))
              (2 + 2)))))

    (define e5
      (term
       (prog ((defvar f (λ (x) (if x g h)))
              (defvar g (λ (x) 42))
              (defvar h (λ (y) 21)))
             ((f tt) 5))))

    (define e6
      (term
       (prog ((defvar f (λ (x) (if x g h)))
              (defvar g (λ (x) 42))
              (defvar h (λ (y) 21)))
             (let ([x 99])
               (set! f x)
               f))))

    (define e7
      (term
       (prog ((defvar f (λ (x) (if x g h)))
              (defvar g (λ (x) 42))
              (defvar h (λ (y) 21)))
             ((λ (x)
                (let ([d (set! x 10)])
                  (set! f (x + 89))
                  f))
              42))))

    (test-equal (term (eval ,e1)) (term 1))
    (test-equal (term (eval ,e2)) (term 2))
    (test-equal (term (eval ,e3)) (term (λ (y) 2)))
    (test-equal (term (eval ,e4)) (term 3))
    (test-equal (term (eval ,e5)) (term 42))
    (test-equal (term (eval ,e6)) (term 99))
    (test-equal (term (eval ,e7)) (term 99))

    (test-equal (term (eval (prog () (1 + 2)))) (term 3))

    (test-equal (term (eval (prog
                             ((defvar first  (λ (a) (λ (b) a)))
                              (defvar second (λ (a) (λ (b) b)))
                              (defvar pair   (λ (a) (λ (b) (λ (f) ((f a) b))))))
                             (((pair 1) 2) second))))
                2)
    ))


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