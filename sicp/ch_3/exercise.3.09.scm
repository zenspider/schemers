#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.9

;; In section *Note 1-2-1:: we used the substitution
;; model to analyze two procedures for computing factorials, a
;; recursive version
;;
;;      (define (factorial n)
;;        (if (= n 1)
;;            1
;;            (* n (factorial (- n 1)))))
;;
;; and an iterative version
;;
;;      (define (factorial n)
;;        (fact-iter 1 1 n))
;;
;;      (define (fact-iter product counter max-count)
;;        (if (> counter max-count)
;;            product
;;            (fact-iter (* counter product)
;;                       (+ counter 1)
;;                       max-count)))
;;
;; Show the environment structures created by evaluating `(factorial
;; 6)' using each version of the `factorial' procedure.(1)

;; I didn't really feel like digging through this with graffle...

;; (factorial 6)        ------------------------. E1: (global) n: 6
;; (* 6 (factorial 5))                          | E2: (global) n: 5
;; (* 6 (* 5 (factorial 4)))                    | E3: (global) n: 4
;; (* 6 (* 5 (* 4 (factorial 3))))              | E4: (global) n: 3
;; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))        | E5: (global) n: 2
;; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))  | E6: (global) n: 1 now reverse
;; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))              | E5: (global) n: 2
;; (* 6 (* 5 (* 4 (* 3 2))))                    | E4: (global) n: 3
;; (* 6 (* 5 (* 4 6)))                          | E3: (global) n: 4
;; (* 6 (* 5 24))                               | E2: (global) n: 5
;; (* 6 120)                                    | E1: (global) n: 6
;; 720          <-------------------------------'

;; (factorial 6)   -----. E1: (global) n: 6
;; (fact-iter   1 1 6)  | E2: (global) p:   1 c: 1 m: 6
;; (fact-iter   1 2 6)  | E3: (global) p:   1 c: 2 m: 6
;; (fact-iter   2 3 6)  | E4: (global) p:   2 c: 3 m: 6
;; (fact-iter   6 4 6)  | E5: (global) p:   6 c: 4 m: 6
;; (fact-iter  24 5 6)  | E6: (global) p:  24 c: 5 m: 6
;; (fact-iter 120 6 6)  | E7: (global) p: 120 c: 6 m: 6
;; (fact-iter 720 7 6)  V E8: (global) p: 720 c: 7 m: 6
;; 720

;; (assert-equal x y)
(done)
