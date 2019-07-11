#lang s-exp "stlc.rkt"

; Compute type of annotated λ:
(def fst (λ ([x Int] [y Bool]) x))
(check-type fst : (-> Int Bool Int))

; Cannot compute type of unannotated λ:
(typecheck-fail (λ (x y) y))

; Check expected type of unannotated λ:
(def snd
  (ann (λ (x y) y)
       (-> Int Bool Bool)))
(check-type snd : (-> Int Bool Bool))

; Expected type doesn't match λ arity:
(typecheck-fail (ann (λ (x y z) z)
                     (-> Int Bool Int)))

; No literals yet:
(typecheck-fail 5)
(typecheck-fail #f)
