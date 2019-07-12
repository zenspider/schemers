#lang turnstile/quicklang

(require turnstile/no-unicode)
(require rackunit/turnstile)

(provide ann λ def
         rec
         let
         if
         (rename-out [λ lambda]
                     [app #%app]
                     [datum #%datum])
         (type-out Bool Int ->)
         (typed-out [not (-> Bool Bool)]
                    [+   (-> Int Int Int)]
                    [*   (-> Int Int Int)]
                    [<=  (-> Int Int Bool)])

         (all-from-out rackunit/turnstile))

(define-base-types Bool Int)
(define-type-constructor -> #:arity >= 1)

;; THE TEMPLATE:
;;
;; (define-typed-syntax form-name
;;   [typed-form-pattern ⇐ expected-type-pattern ; confirmed/verified
;;    ≫
;;    premisses ...             ; [[x ≫ x- : s] ... ⊢ e ≫ e- ⇐ t]
;;    ----------                ; ^^ maps to x- and uses them after
;;                              ; vv doesn't need to declare a type
;;    conclusion]               ; [⊢ (λ- (x- ...) e-)]
;;   [typed-form-pattern                         ; computed
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

;; (define-base-types Int) gives:
;;
;;   Int  : just the name
;;   Int? : -> syntax? boolean? (phase 1) "am I an int?"
;;   Int- : internals
;;   ~Int : pattern for macros
;; ... and bools

;; (define-type-constructor -> #:arity >= 1) gives:
;;
;;   ->  : just the name
;;   ->? : -> syntax? boolean? "am I a function?"
;;   ->- : X
;;   ~-> : eg (~-> ~Int ~Int)    (pattern w/ hard types)
;;         or (~-> dom cod)      (pattern w/ type vars)

;; TRANSLATION:
;;   [[x ≫ x- : s] ... ⊢ e ≫ e- ⇐ t]
;; "given [extending env x's translated to x- with type s],
;;  prove e translated to e- confirms type t"

;; ---------------------------------------------------------------------

(define-typed-syntax datum
  [(_ . x:integer)
   ≫
   --------------------
   [⊢ (#%datum- . x) ⇒ Int]]
  [(_ . x:boolean)
   ≫
   --------------------
   [⊢ (#%datum- . x) ⇒ Bool]]
  [(_ . x) ≫ #:fail-when #t "Unsupported literal!"
   --------------------
   [⊢ (void-)]])

;;       Δ ⊢ ƒ ≫ ƒ- ⇒ (-> σ ... τ)
;;           Δ ⊢ e ≫ e- ⇐ σ ...
;; --------------------------------------
;; Δ ⊢ (ƒ e ...) ≫ (#%app ƒ- e- ...) ⇒ τ

(define-typed-syntax app
  [(_ ƒ:expr e:expr ...)
   ≫
   [⊢ ƒ ≫ ƒ- ⇒ (~-> σ ... τ)]
   #:fail-unless (= (stx-length #'(σ ...))
                    (stx-length #'(e ...)))
   "wrong number of args for call"
   [⊢ e ≫ e- ⇐ σ] ...
   ----------
   [⊢ (#%app- ƒ- e- ...) ⇒ τ]])

;; ------------
;; Δ ≻ ...dunno

(define-typed-syntax def
  [(_ x:id e:expr)
   ≫
   -------------------------------
   [≻ (define-typed-variable x e)]]

  [(_ x:id τ:type e:expr)
   ≫
   [⊢ e ≫ e- ⇐ τ.norm]
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

;; IF=>
;;                ∆ ⊢ e1 ≫ e1- ⇒ Bool
;;                ∆ ⊢ e2 ≫ e2- ⇒ τ
;;                ∆ ⊢ e3 ≫ e3- ⇒ τ
;; ---------------------------------------------
;; ∆ ⊢ (if e1 e2 e3) ≫ (if- e1- e2- e3-) ⇒ τ
;;
;; IF<=
;;                ∆ ⊢ e1 ≫ e1- ⇐ Bool
;;                ∆ ⊢ e2 ≫ e2- ⇐ τ
;;                ∆ ⊢ e3 ≫ e3- ⇐ τ
;; ---------------------------------------------
;; ∆ ⊢ (if e1 e2 e3) ≫ (if- e1- e2- e3-) ⇐ τ

(define-typed-syntax if
  [(_ e1 e2 e3)
   ≫
   [⊢ e1 ≫ e1- ⇒ Bool]
   [⊢ e2 ≫ e2- ⇒ τ1]
   [⊢ e3 ≫ e3- ⇒ τ2]
   #:fail-unless (type=? #'τ1 #'τ2) "fucked up types"
   ----------
   [⊢ (if- e1- e2- e3-) ⇒ τ1]]

  [(_ e1 e2 e3)
   ⇐ τ
   ≫
   [⊢ e1 ≫ e1- ⇐ Bool]
   [⊢ e2 ≫ e2- ⇐ τ]
   [⊢ e3 ≫ e3- ⇐ τ]
   ----------
   [⊢ (if- e1- e2- e3-)]])

;;             ∆,[x>>x':∂],... ¬ e >> e' => †
;; ---------------------------------------------------------
;; ∆ ⊢ (λ ([x ∂] ...) e) ≫ (λ' (x' ...) e') ⇒ (-> ∂ ... †)

(define-typed-syntax λ
  [(_ (x:id ...) e:expr) ;; confirmed/verified
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

(define-syntax (rec- stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     #'(letrec- ([x e]) x)]))

;;    Δ,[x ≫ x- : τ] ⊢ e ≫ e- ⇐ τ
;; ----------------------------------
;; Δ ⊢ (rec x e) ≫ (rec- x- e-) ⇐ τ
;;
;;    Δ,[x ≫ x- : τ] ⊢ e ≫ e- ⇐ τ
;; ----------------------------------
;; Δ ⊢ (rec x τ e) ≫ (rec- x- e-) ⇒ τ

(define-typed-syntax rec
  [(_ x:id e:expr)
   ⇐ τ
   ≫
   [[x ≫ x- : τ] ⊢ e ≫ e- ⇐ τ]
   ----------------------------
   [⊢ (rec- x- e-)]]

  [(_ x:id τ:type e:expr)
   ≫
   [[x ≫ x- : τ] ⊢ e ≫ e- ⇐ τ]
   ----------------------------
   [⊢ (rec- x- e-) ⇒ τ]]
  )

;; Δ ⊢ v ≫ v- ⇒ σ
;; Δ,[k ≫ k- : σ] ⊢ e >> e- ⇒ τ
;; --------------------------------------------------
;; Δ ⊢ (let ([k v] e) ≫ (let- ([k- v-]) e-) ⇒ τ
;;
;; Δ ⊢ v ≫ v- ⇒ σ
;; Δ,[k ≫ k- : σ] ⊢ e >> e- ⇐ τ
;; --------------------------------------------------
;; Δ ⊢ (let ([k v] e) ≫ (let- ([k- v-]) e-) ⇐ τ

(define-typed-syntax let
  [(_ ([k:id v:expr] ...) e:expr)
   ≫
   [⊢ v ≫ v- ⇒ σ] ...
   [[k ≫ k- : σ] ... ⊢ e ≫ e- ⇒ τ]
   ----------
   [⊢ ((lambda- (k- ...) e-) v- ...) ⇒ τ]]

  [(_ ([k:id v:expr] ...) e:expr)
   ⇐ τ
   ≫
   [⊢ v ≫ v- ⇒ σ] ...
   [[k ≫ k- : σ] ... ⊢ e ≫ e- ⇐ τ]
   ----------
   [⊢ ((lambda- (k- ...) e-) v- ...) ⇒ τ]])
