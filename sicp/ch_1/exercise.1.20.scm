
;;; Exercise 1.20:

;; The process that a procedure generates is of course dependent on
;; the rules used by the interpreter. As an example, consider the
;; iterative `gcd' procedure given above. Suppose we were to interpret
;; this procedure using normal-order evaluation, as discussed in
;; section *Note 1-1-5::. (The normal-order-evaluation rule for `if'
;; is described in *Note Exercise 1-5::.) Using the substitution
;; method (for normal order), illustrate the process generated in
;; evaluating `(gcd 206 40)' and indicate the `remainder' operations
;; that are actually performed. How many `remainder' operations are
;; actually performed in the normal-order evaluation of `(gcd 206
;; 40)'? In the applicative-order evaluation?

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; applicative order:

(gcd 206 40)                            ; > (remainder 206 40)
(gcd 40 6)                              ; > (remainder 40 6)
(gcd 6 4)                               ; > (remainder 6 4)
(gcd 4 2)                               ; > (remainder 4 2)
(gcd 2 0)
2

;; 4 calls to remainder
;; normal order:

;; shorten for readibility... not that it really helps... ugh
(define g gcd)
(define r remainder)

;; step 1
(g 206 40)                              ; expand/eval/substitute to:
(if (= 40 0) 206 (g 40 (r 206 40)))     ; expands
(if #f 206 (g 40 (r 206 40)))           ; evals and substitutes, and so on:

;; step 2
(g 40 (r 206 40))
(if (= (r 206 40) 0) 40 (g (r 206 40) (r 40 (r 206 40))))
(if (= 6 0) 40 (g (r 206 40) (r 40 (r 206 40))))
(if #f 40 (g (r 206 40) (r 40 (r 206 40))))

;; step 3
(g (r 206 40) (r 40 (r 206 40)))
(if (= (r 40 (r 206 40)) 0)
    (r 206 40)
    (g (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(if (= 4 0)
    (r 206 40)
    (g (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(if #f
    (r 206 40)
    (g (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))

;; step 4
(g (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
(if (= (r (r 206 40) (r 40 (r 206 40))) 0)
    (r 40 (r 206 40))
    (g (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
(if (= 2 0)
    (r 40 (r 206 40))
    (g (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))
(if #f
    (r 40 (r 206 40))
    (g (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))))

;; step 5
(g (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
(if (= (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (g (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
(if (= 0 0)
    (r (r 206 40) (r 40 (r 206 40)))
    (g (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))
(if #t
    (r (r 206 40) (r 40 (r 206 40)))
    (g (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))) (r (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))))

;; step 6 - finally done
(r (r 206 40) (r 40 (r 206 40)))
2

;; so... 18 calls to remainder? I think I counted that right.

;; and so on... ugh
