#lang r5rs

;;; Exercise 1.11:

;; A function f is defined by the rule that
;;    f(n) = n if n < 3
;; and
;;    f(n) = 1*f(n - 1) + 2*f(n - 2) + 3*f(n - 3) if n >= 3.
;;
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of an iterative process.

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0) b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 10)                                ; 55

(define (f-recursive n)
  (if (< n 3) n
      (+ (* 1 (f-recursive (- n 1)))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(f-recursive 1)                         ; 1
(f-recursive 2)                         ; 2
(f-recursive 3)                         ; 4
(f-recursive 4)                         ; 11
(f-recursive 5)                         ; 25
(f-recursive 6)                         ; 59

;; n :: 1 *  a + 2 * b + 3 * c = f(n)
;; ===============================
;; 0 ::                        =  0
;; 1 ::                        =  1
;; 2 ::                        =  2
;; --------------------------------
;; 3 ::      2 +     1 +     0 =  4
;; 4 ::      4 +     2 +     1 = 11
;; 5 ::     11 +     4 +     2 = 25
;; 6 ::     25 +    11 +     4 = 59

(define (f-iterative n)
  (define (f-iter a b c count)
    (if (= count 0) a
        (f-iter (+ (* 1 a) (* 2 b) (* 3 c)) a b (- count 1))))
  (if (< n 3) n
      (f-iter 2 1 0 (- n 2))))

(f-iterative 1)                         ; 1
(f-iterative 2)                         ; 2
(f-iterative 3)                         ; 4
(f-iterative 4)                         ; 11
(f-iterative 5)                         ; 25
(f-iterative 6)                         ; 59

;; my first version, works but... ugh! It took me a while to realize
;; that the f-iter should only be used for n>3. dur.

(define (f-iterative-ugly n)
  (define (f-iter a b c count)
    (cond ((< count 3) count)
          ((= count 3) (+ a (* 2 b) (* 3 c)))
          (else
           (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 n))

;; time = 1:05
