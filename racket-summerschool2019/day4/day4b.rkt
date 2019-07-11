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

(check-type 5             : Int -> 5)
(check-type (+ 4 5)       : Int -> 9)
(check-type (+ 4 (+ 5 6)) : Int -> 15)

(typecheck-fail (+ 5 #t))
(typecheck-fail (+ 5))

(def - (-> Int Int Int)
  (λ (x y) (+ x (* y -1))))

(check-type -             : (-> Int Int Int))
(check-type (- 12 7)      : Int -> 5)

(typecheck-fail (- #t #f))
