#lang racket

;;; Exercise 1.43:

;; If f is a numerical function and n is a positive integer, then we
;; can form the nth repeated application of f, which is defined to be
;; the function whose value at x is f(f(...(f(x))...)). For example,
;; if f is the function x |-> x + 1, then the nth repeated application
;; of f is the function x |-> x + n. If f is the operation of squaring
;; a number, then the nth repeated application of f is the function
;; that raises its argument to the 2^nth power. Write a procedure that
;; takes as inputs a procedure that computes f and a positive integer
;; n and returns the procedure that computes the nth repeated
;; application of f. Your procedure should be able to be used as
;; follows:
;; 
;;      ((repeated square 2) 5)
;;      625
;; 
;; Hint: You may find it convenient to use `compose' from *Note
;; Exercise 1-42::.

(define (square n) (* n n))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity n) n)

((compose square square) 5)             ; 625

((compose (compose square square) square) 5) ; 390625

((lambda (x) ((compose square square) x)) 5) ; 625

(define (repeated-twice f)
  (lambda (x) ((compose f f) x)))

((repeated-twice square) 5)             ; 625

(define (repeated f n)
  (cond ((= n 0) f)
        ((= n 1) (compose f identity))
        (else (repeated (compose f f) (- n 2)))))

(define (repeated-i f n)                ; I'm sure I can do this cleaner
  (define (iterate f- n)
    (cond ((= n 0) f-)
          ((= n 1) (compose f- f))
          (else (iterate (compose f f) (- n 1)))))
  (iterate f (- n 1)))                  ; HACK: - here seems wrong

((repeated square 1) 5)                 ; 25
((repeated square 2) 5)                 ; 625
((repeated square 3) 5)                 ; this is wrong

((repeated-i square 1) 5)               ; 25
((repeated-i square 2) 5)               ; 625
((repeated-i square 3) 5)               ; 390625
