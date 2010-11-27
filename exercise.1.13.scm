#lang r5rs

;;; Exercise 1.13:

;; Prove that Fib(n) is the closest integer to phi^n/sqrt(5), where
;;
;;   phi = (1 + sqrt(5)) / 2.
;;
;; Hint: Let
;;
;;   psi = (1 - sqrt(5)) / 2.
;;
;; Use induction and the definition of the Fibonacci numbers (see
;; section *Note 1-2-2::) to prove that
;;
;;   fib(n) = (phi^n - psi^n)/sqrt(5).

(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 2)) (fib (- n 1)))))

1.618033988749895

(define s5  (sqrt 5))
(define phi (/ (+ 1 s5) 2))
(define psi (/ (- 1 s5) 2))

;; 90 minutes in... fuck this shit:
;; http://en.wikipedia.org/wiki/Golden_ratio#Relationship_to_Fibonacci_sequence

(fib 0)                                 ;  0
(fib 1)                                 ;  1
(fib 2)                                 ;  1 (/ 1 1)     1
(fib 3)                                 ;  2 (/ 2 1.0)   2.0
(fib 4)                                 ;  3 (/ 3 2.0)   1.5
(fib 5)                                 ;  5 (/ 5 3.0)   1.6667
(fib 6)                                 ;  8 (/ 8 5.0)   1.6
(fib 7)                                 ; 13 (/ 13 8.0)  1.625
(fib 8)                                 ; 21 (/ 21 13.0) 1.6153846
(fib 9)                                 ; 34 (/ 34 21.0) 1.619047619047619
(fib 10)                                ; 55 (/ 55 34.0) 1.6176470588235294

;; so, if the ratio of fib(n)/fib(n-1) converges on phi

(define (fib-phi n) (/ (- (expt (/ (+ 1 s5) 2) n)
                          (expt (/ (- 1 s5) 2) n))
                       s5))



;; phi^n               / sqrt(5)
;; or
;; phi^n - (1 - phi)^n / sqrt(5)

;; (1 + sqrt(5) / 2)^n               / sqrt(5)
;; or
;; (1 + sqrt(5) / 2)^n - (1 - (1 + sqrt(5) / 2))^n / sqrt(5)

;; fuck this shit... I'm tired.