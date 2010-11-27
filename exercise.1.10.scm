#lang r5rs

;;; Exercise 1.10:

;; The following procedure computes a mathematical function called
;; Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; What are the values of the following expressions?

(A 1 10)                                ; 1024

(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024

(A 2 4)                                 ; 65536

;; umm... no

(A 3 3)                                 ; 65536

;; umm... no, again.

;; Consider the following procedures, where `A' is the procedure
;; defined above:

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

;; Give concise mathematical definitions for the functions computed by
;; the procedures `f', `g', and `h' for positive integer values of n.
;; For example, `(k n)' computes 5n^2.

;; (f n) == (* 2 n)

(f 1)                                   ; 2
(f 2)                                   ; 4
(f 3)                                   ; 6
(f 4)                                   ; 8

;; (g n) == (expt 2 n)

(g 1)                                   ; 2
(g 2)                                   ; 4
(g 3)                                   ; 8
(g 4)                                   ; 16

(define e expt)                         ; alias for readability

;; (h n) == (e 2 (h (- n 1))) apparently known as a "power tower"

(h 1)                ;                   2       = (e 2 1)     = 2
(h 2)                ;                (e 2 2)    = (e 2 2)     = 4
(h 3)                ;           (e 2 (e 2 2))   = (e 2 4)     = 16
(h 4)                ;      (e 2 (e 2 (e 2 2)))  = (e 2 16)    = 65536
;; (h 5)             ; (e 2 (e 2 (e 2 (e 2 2)))) = (e 2 65536) = HUUGE and slow
