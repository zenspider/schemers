#lang racket

;;; Exercise 1.41:

;; Define a procedure `double' that takes a procedure of arity 1
;; as argument and returns a procedure that applies the original
;; procedure twice. For example, if `inc' is a procedure that adds 1
;; to its argument, then `(double inc)' should be a procedure that
;; adds 2. What value is returned by
;; 
;;      (((double (double double)) inc) 5)

(define (double f) (lambda (n) (f (f n))))
(define (inc n) (+ n 1))

(define i inc)
(define d double)

((double inc) 1)                        ; 3
;; ((d i) 1)
;; ((lambda (n) (define f i) (f (f n))) 1)
;; ((lambda (n) (i (i n))) 1)
;; (i (i 1))
;; (i (+ 1 1))
;; (i 2)
;; (+ 2 1)
;; 3

((double inc) 2)                        ; 4
;; ((d i) 2)
;; ((lambda (n) (define f i) (f (f n))) 2)
;; ((lambda (n) (i (i n))) 2)
;; (i (i 2))
;; (i (+ 2 1))
;; (i 3)
;; (+ 3 1)
;; 4

;; ugh
(((double double) inc) 1)               ; 5
(((d d) i) 1)
(((lambda (n) (define f d) (f (f n))) i) 1)
(((lambda (n) (d (d n))) i) 1)
((d (d i)) 1)
((d (lambda (n) (define f i) (f (f n)))) 1)
((d (lambda (n) (i (i n)))) 1)
((lambda (n) (define f (lambda (x) (i (i x)))) (f (f n))) 1)
((lambda (n) ((lambda (x) (i (i x))) ((lambda (x) (i (i x))) n))) 1)
((lambda (n) ((lambda (x) (i (i x))) (i (i n)))) 1)
((lambda (n) (i (i (i (i n))))) 1)
(i (i (i (i 1))))
(i (i (i 2)))
(i (i 3))
(i 4)
5

((double (double (double inc))) 1)      ; 9

(((double (double double)) inc) 5)      ; 21
;; use aliases
(((d (d d)) i) 1)
;; call d w/ d
(((d (lambda (a) (define f d) (f (f a)))) i) 5)
;; apply d to f
(((d (lambda (a) (d (d a)))) i) 5)
;; call d w/ lambda a
(((lambda (b) (define f (lambda (a) (d (d a)))) (f (f b))) i) 5)
;; apply lambda a to f
(((lambda (b) ((lambda (a) (d (d a))) ((lambda (a2) (d (d a2))) b))) i) 5)
;; apply i to b
(((lambda (a) (d (d a))) ((lambda (a2) (d (d a2))) i)) 5)
;; apply i to a2
(((lambda (a) (d (d a))) (d (d i))) 5)
;; call lambda c w/ i
(((lambda (a) (d (d a))) (d (lambda (c) (define f i) (f (f c))))) 5)
;; apply i to f
(((lambda (a) (d (d a))) (d (lambda (c) (i (i c))))) 5)
;; call d w/ lambda c
(((lambda (a) (d (d a)))
  (lambda (e) (define f (lambda (c) (i (i c)))) (f (f e)))) 5)
;; apply lambda c to f
(((lambda (a) (d (d a)))
  (lambda (e) ((lambda (c) (i (i c)))
               ((lambda (c2) (i (i c2))) e)))) 5)
;; apply lambda e to a
((d (d (lambda (e) ((lambda (c) (i (i c)))
                    ((lambda (c2) (i (i c2))) e))))) 5)
;; call d w/ lambda e
((d (lambda (g) (define f (lambda (e) ((lambda (c) (i (i c)))
                                       ((lambda (c2) (i (i c2))) e))))
            (f (f g)))) 5)
;; apply e to f
((d (lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                              ((lambda (c2) (i (i c2))) e)))
                 ((lambda (e2) ((lambda (c3) (i (i c3)))
                                ((lambda (c4) (i (i c4))) e2))) g)))) 5)
;; call d w/ lambda g
((lambda (h) (define f (lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                                                 ((lambda (c2) (i (i c2))) e)))
                                    ((lambda (e2) ((lambda (c3) (i (i c3)))
                                                   ((lambda (c4) (i (i c4)))
                                                    e2)))
                                     g))))
         (f (f h))) 5)
;; apply lambda g to f
((lambda (h) ((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                                        ((lambda (c2) (i (i c2))) e)))
                           ((lambda (e2) ((lambda (c3) (i (i c3)))
                                          ((lambda (c4) (i (i c4))) e2))) g)))
              ((lambda (g2) ((lambda (e3) ((lambda (c5) (i (i c5)))
                                           ((lambda (c6) (i (i c6))) e3)))
                             ((lambda (e4) ((lambda (c7) (i (i c7)))
                                            ((lambda (c8) (i (i c8))) e4)))
                              g2))) h))) 5)
;; call lambda h w/ 5
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (g2) ((lambda (e3) ((lambda (c5) (i (i c5)))
                              ((lambda (c6) (i (i c6))) e3)))
                ((lambda (e4) ((lambda (c7) (i (i c7)))
                               ((lambda (c8) (i (i c8))) e4))) g2))) 5))
;; call lambda g2 w/ 5
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  ((lambda (e4) ((lambda (c7) (i (i c7)))
                 ((lambda (c8) (i (i c8))) e4))) 5)))
;; call lambda e4 w/ 5
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  ((lambda (c7) (i (i c7)))
   ((lambda (c8) (i (i c8))) 5))))
;; call lambda c8 w/ 5
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  ((lambda (c7) (i (i c7)))
   (i (i 5)))))
;; call lambda c8 w/ 5
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  ((lambda (c7) (i (i c7))) (i (i 5)))))
;; call i 5, call subsequent +
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  ((lambda (c7) (i (i c7)))
   (i 6))))
;; call i 6, call +
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  ((lambda (c7) (i (i c7)))
   7)))
;; call lambda c7 w/ 7
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  (i (i 7))))
;; call i 7, call +
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3)))
  (i 8)))
;; call i 8, call +
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (e3) ((lambda (c5) (i (i c5)))
                ((lambda (c6) (i (i c6))) e3))) 9))
;; call lambda e3 w/ 9
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (c5) (i (i c5)))
  ((lambda (c6) (i (i c6))) 9)))
;; call lambda c6 w/ 9
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (c5) (i (i c5)))
  (i (i 9))))
;; call i 9
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (c5) (i (i c5)))
  (i 10)))
;; call i 10
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 ((lambda (c5) (i (i c5))) 11))
;; call lambda c5 w/ 11
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 (i (i 11)))
;; call i 11
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 (i 12))
;; call i 12
((lambda (g) ((lambda (e) ((lambda (c) (i (i c)))
                           ((lambda (c2) (i (i c2))) e)))
              ((lambda (e2) ((lambda (c3) (i (i c3)))
                             ((lambda (c4) (i (i c4))) e2))) g)))
 13)
;; call lambda g w/ 13
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 ((lambda (e2) ((lambda (c3) (i (i c3)))
                ((lambda (c4) (i (i c4))) e2))) 13))
;; call lambda e2 w/ 13
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 ((lambda (c3) (i (i c3)))
  ((lambda (c4) (i (i c4))) 13)))
;; call lambda c4 w/ 13
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 ((lambda (c3) (i (i c3)))
  (i (i 13))))
;; call i 13
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 ((lambda (c3) (i (i c3)))
  (i 14)))
;; call i 14
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 ((lambda (c3) (i (i c3))) 15))
;; call lambda c3 w/ 15
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 (i (i 15)))
;; call i 15
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e)))
 (i 16))
;; call i 16
((lambda (e) ((lambda (c) (i (i c)))
              ((lambda (c2) (i (i c2))) e))) 17)
;; FINALLY! call lambda e w/ 17
((lambda (c) (i (i c)))
 ((lambda (c2) (i (i c2))) 17))
;; call lambda c2 w/ 17
((lambda (c) (i (i c)))
 (i (i 17)))
;; call i 17
((lambda (c) (i (i c)))
 (i 18))
;; call i 18
((lambda (c) (i (i c))) 19)
;; call lambda c w/ 19
(i (i 19))
;; call i 19
(i 20)
;; call i 20
21
