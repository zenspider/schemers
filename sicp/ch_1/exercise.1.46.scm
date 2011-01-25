
;;; Exercise 1.46:

;; Several of the numerical methods described in this chapter are
;; instances of an extremely general computational strategy known as
;; "iterative improvement". Iterative improvement says that, to
;; compute something, we start with an initial guess for the answer,
;; test if the guess is good enough, and otherwise improve the guess
;; and continue the process using the improved guess as the new guess.
;; Write a procedure `iterative-improve' that takes two procedures as
;; arguments: a method for telling whether a guess is good enough and
;; a method for improving a guess. `Iterative-improve' should return
;; as its value a procedure that takes a guess as argument and keeps
;; improving the guess until it is good enough. Rewrite the `sqrt'
;; procedure of section *Note 1-1-7:: and the `fixed-point' procedure
;; of section *Note 1-3-3:: in terms of `iterative-improve'.

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (iterate guess n)
      (if (good-enough? guess n) guess
          (iterate (improve guess n) n)))
    (iterate 1.0 x)))

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define sqrt (iterative-improve
              (lambda (guess x) (< (abs (- (square guess) x)) 0.001))
              (lambda (guess x) (average guess (/ x guess)))))

(sqrt 25)

;; fixed-point... don't care
