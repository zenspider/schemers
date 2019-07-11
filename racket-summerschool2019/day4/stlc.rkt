#lang turnstile/quicklang

(require turnstile/no-unicode)
(require rackunit/turnstile)

(provide ann λ def
         (rename-out [λ lambda])
         (type-out Bool Int ->)
         (typed-out [not (-> Bool Bool)]
                    [+   (-> Int Int Int)]
                    [<=  (-> Int Int Bool)])

         (all-from-out rackunit/turnstile))

(define-base-types Bool Int)
(define-type-constructor -> #:arity >= 1)

;; THE TEMPLATE:
;;
;; (define-typed-syntax form-name
;;   [typed-form-pattern ⇐ expected-type-pattern
;;    ≫
;;    premisses ...             ; [[x ≫ x- : s] ... ⊢ e ≫ e- ⇐ t]
;;    ----------                ; ^^ maps to x- and uses them after
;;                              ; vv doesn't need to declare a type
;;    conclusion]               ; [⊢ (λ- (x- ...) e-)]
;;   [typed-form-pattern
;;    ≫
;;    premisses ...             ; [[x ≫ x- : s] ... ⊢ e ≫ e- ⇒ t]
;;    ----------                ; ^^ same as above, but pushes type
;;                              ; vv pushes down (⇒) a type
;;    conclusion]               ; [⊢ (λ- (x- ...) e-) ⇒ (-> s ... t)]
;;   )

;; GLOSSARY:
;;
;; succ       ≻  | sugar for "no type here, keep going"
;; vdash      ⊢  | prove
;; :             | has type
;; gg         ≫  | elaborates (translated to)
;; Leftarrow  ⇐  | confirms
;; Rightarrow ⇒  | computes

;; TRANSLATION:
;;   [[x ≫ x- : s] ... ⊢ e ≫ e- ⇐ t]
;; "given [extending env x's translated to x- with type s],
;;  prove e translated to e- confirms type t"

;; ---------------------------------------------------------------------

;; ------------
;; Δ ≻ ...dunno

(define-typed-syntax def
  [(_ x:id e:expr)
   ≫
   -------------------------------
   [≻ (define-typed-variable x e)]]

  [(_ x:id τ:type e:expr)
   ≫
   [⊢ e ≫ -e ⇐ τ.norm]
   -------------------------------
   [≻ (define-typed-variable x e- ⇐ τ.norm)]])

;;     Δ ⊢ e ≫ e' ⇐ τ
;; -----------------------
;; Δ ⊢ (ann e τ) ≫ e ⇒ τ

(define-typed-syntax ann
  [(_ e:expr τ:type)
   ≫
   [⊢ e ≫ e- ⇐ τ.norm]
   --------------------
   [⊢ e- ⇒ τ.norm]])

;;                      x'... fresh
;;             ∆,[x>>x':∂],... ¬ e >> e' => †
;; ---------------------------------------------------------
;; ∆ ⊢ (λ ([x ∂] ...) e) ≫ (λ' (x' ...) e') ⇒ (-> ∂ ... †)

(define-typed-syntax λ
  [(_ (x:id ...) e:expr) ;; confimed form
   ⇐ (~-> s ... t)
   ≫
   #:fail-when (check-duplicate-identifier (stx->list #'(x ...)))
   "repeated formal parameter name"
   #:fail-unless (= (stx-length #'(x ...))
                    (stx-length #'(s ...)))
   "wrong number of formal parameters for expected arrow type"
   ;;
   [[x ≫ x- : s] ... ⊢ e ≫ e- ⇐ t]
   --------------------------------
   [⊢ (λ- (x- ...) e-)]]

  [(_ ([x:id σ:type] ...) e:expr) ;; computed form
   ≫
   #:fail-when (check-duplicate-identifier (stx->list #'(x ...)))
   "repeated formal parameter name"
   #:with (s ...) #'(σ.norm ...) ; translate to normals

   [[x ≫ x- : s] ... ⊢ e ≫ e- ⇒ t]
   ------------------------------------
   [⊢ (λ- (x- ...) e-) ⇒ (-> s ... t)]])

;; gives:
;;
;;   Int  : just the name
;;   Int? : -> syntax? boolean? (phase 1) "am I an int?"
;;   Int- : internals
;;   ~Int : pattern for macros
;; ... and bools

;; gives:
;;
;;   ->  : just the name
;;   ->? : -> syntax? boolean? "am I a function?"
;;   ->- : X
;;   ~-> : eg (~-> ~Int ~Int)    (pattern w/ hard types)
;;         or (~-> dom cod)      (pattern w/ type vars)

;; (define-syntax (count-arrows stx)
;;   (syntax-parse stx
;;     [(_ τ:type)
;;      #:with (~-> dom ... cod) #'τ.norm
;;      #'(+ 1 (count-arrows dom) ... (count-arrows cod))]
;;     [_ #'0]))
;;
;; (count-arrows (-> Int (-> Bool Int)))
;; (count-arrows (-> Int (-> Bool Int) (-> Bool)))

#|

e ::= z
    | b
    | (λ (x ...) e)
    | (λ ([x t] ...) e)
    | (let [x e] ...) e)
    | (f e ...)
    | x
    | (rec x e)
    | (rec x t e)

∆ ::= null,
    | ∆, [x >> x' : t]

----------------------------------------------------------------------

e ::= z
    | b
    | (λ (x ...) e)
    | (f e ...)
    | (let [x e] e)
    | (rec x e)

[x>>x' : t] « ∆
---------------
 ∆ ¬ x>>x' => t

IF ->                    ∆ ¬ e1>>e1' : ∂
                         ∆ ¬ e2>>e2' : †
                         ∆ ¬ e3>>e3' : †
          ---------------------------------------------
          ∆ ¬ (if e1 e2 e3) : † >> (if e1' e2' e3') : †

|#

;; (define-syntax (syntax-parse0 stx)
;;   (syntax-parse stx
;;     #:literals (syntax)
;;     [(_ #'e0 [p e1 ... er] ...+)
;;      #'(let-syntax ([m (λ (stx)
;;                          (syntax-parse stx
;;                            [(_ p) e1 ... #`'#,er] ...))])
;;          (m e0))]))
;;
;; (syntax-parse0 #'Int
;;                [t:type {type->str #'t}])
;;
;; (syntax-parse0 #'(-> Int Bool)
;;                [τ:type #:with (~-> dom cod) #'τ.norm
;;                        (type->str #'cod)])
