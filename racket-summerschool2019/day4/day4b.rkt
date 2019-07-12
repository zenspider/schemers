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

;; Finds the factorial of an integer, weirdly.
(def fact
  (rec self (-> Int Int)
       (λ (n)
         (let ([condition (<= n 1)]
               [n         (- n 1)])
           (* (+ n 1) (if condition 1 (self n)))))))

(check-type (fact 5) : Int -> 120)

;; (iter n f x) applies `f` to `x`, `n` times.
(def iter (-> Int (-> Int Int) Int Int)
  (rec self
       (λ (n f x)
         (if (<= n 0)
             x
             (self (- n 1) f (f x))))))

;; Integer exponentiation using `iter`.
(def expt (-> Int Int Int)
  (λ (n m)
    (iter m (λ (acc) (* n acc)) 1)))

(check-type (expt 2 10) : Int -> 1024)
