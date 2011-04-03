#!/usr/bin/env csi -s

(use test amb amb-extras)
(use numbers)

;; wtf:
(map (lambda (f) (f 5.0)) (list round truncate floor))
;; # => (5.0 5.0 5.0)

;; (put 'amb-let 'scheme-indent-function 1)
(define-syntax amb-let
  (syntax-rules ()
    [(amb-let ([var expr ...] ...) . body)
     (amb-collect (let ([var (amb expr ...)] ...) . body))]))

;;; Exercise 4.36

;; *Note Exercise 3-69:: discussed how to generate the stream of _all_
;; Pythagorean triples, with no upper bound on the size of the
;; integers to be searched. Explain why simply replacing
;; `an-integer-between' by `an-integer-starting-from' in the procedure
;; in *Note Exercise 4-35:: is not an adequate way to generate
;; arbitrary Pythagorean triples. Write a procedure that actually will
;; accomplish this. (That is, write a procedure for which repeatedly
;; typing `try-again' would in principle eventually generate all
;; Pythagorean triples.)

;; A: an-integer-starting-from would never end and therefore never
;;    backtrack.

;; I don't like the names
(define (amb-ints-from n)
  (amb n (amb-ints-from (+ n 1))))

(define (amb-ints-between low high)
  (amb-assert (<= low high))
  (amb low (amb-ints-between (+ low 1) high)))

;; completely stolen (and reworked) from aja
(define (pythagorean-triples . n)
  (define (helper max)
    (let ((i max))
      (let ((j (amb-ints-between 1 max)))
        (let ((k (sqrt (+ (* i i) (* j j)))))
          (amb-assert (integer? k)) ;; not an int in chicken. *shrug*
          (list j i k)))))
  (amb (helper (if (null? n) (amb-ints-from 1) (amb-ints-between 1 (car n))))
       (amb)))

;; I clearly don't fully grok call/cc yet...

;; (define (i x) x)
;; (amb-let ((a (pythagorean-triples 25)))
;;   (let ((l '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17) (12 16 20)))
;;         (expected #f))
;;     (set! expected (car l))
;;     (set! l (cdr l))
;;     expected
;;     #;(test expected (i a))))
