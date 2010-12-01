#lang racket

;;; Exercise 1.37:

;;   a. An infinite "continued fraction" is an expression of the form
;; 
;;                      N[1]
;;           f = ---------------------
;;                          N[2]
;;               D[1] + ---------------
;;                              N[3]
;;                     D[2] + ---------
;;                           D[3] + ...
;; 
;;      As an example, one can show that the infinite continued
;;      fraction expansion with the n[i] and the D[i] all equal to 1
;;      produces 1/[phi], where [phi] is the golden ratio (described
;;      in section *Note 1-2-2::).  One way to approximate an
;;      infinite continued fraction is to truncate the expansion
;;      after a given number of terms.  Such a truncation--a
;;      so-called finite continued fraction "k-term finite continued
;;      fraction"--has the form
;; 
;;                  N[1]
;;           -----------------
;;                     N[2]
;;           D[1] + -----------
;;                 ...    N[K]
;;                     + -----
;;                        D[K]
;; 
;;      Suppose that `n' and `d' are procedures of one argument (the
;;      term index i) that return the n[i] and D[i] of the terms of the
;;      continued fraction.  Define a procedure `cont-frac' such that
;;      evaluating `(cont-frac n d k)' computes the value of the
;;      k-term finite continued fraction.  Check your procedure by
;;      approximating 1/[phi] using
;; 
;;           (cont-frac (lambda (i) 1.0)
;;                      (lambda (i) 1.0)
;;                      k)
;; 
;;      for successive values of `k'.  How large must you make `k' in
;;      order to get an approximation that is accurate to 4 decimal
;;      places?

(define (cont-frac n d i)
  (if (< i 1) 0
      (/ (n i)
         (+ (d i) (cont-frac n d (- i 1))))))

(define (phi-approx k)
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

(define (cont-frac-test places)
  (let ((error (expt 10 (- (+ places 1))))
        (phi   (/ (+ 1 (sqrt 5)) 2)))
    (define (iterate n)
      (if (< (abs (- phi (phi-approx n))) error)
          n
          (iterate (+ n 1))))
    (iterate 1)))

(cont-frac-test 4)                      ; 13
(phi-approx 13)                         ; 1.6180257510729614
(/ (+ 1 (sqrt 5)) 2)                    ; 1.618033988749895

;;   b. If your `cont-frac' procedure generates a recursive process,
;;      write one that generates an iterative process.  If it
;;      generates an iterative process, write one that generates a
;;      recursive process.

;; seriously? are MIT students this bad at this?

(define (cont-frac-r n d i)
  (define (iterate i fraction)
    (if (< i 1) fraction
        (iterate (- i 1) 
                 (/ (n i)
                    (+ (d i) fraction)))))
  (iterate i 0))

(define (phi-approx-r k)
  (/ 1 (cont-frac-r (lambda (i) 1.0) (lambda (i) 1.0) k)))

(phi-approx-r 13)                       ; 1.6180257510729614
