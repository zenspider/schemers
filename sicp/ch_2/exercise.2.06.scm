#lang racket

;;; Exercise 2.6:

;; In case representing pairs as procedures wasn't mind-boggling
;; enough, consider that, in a language that can manipulate
;; procedures, we can get by without numbers (at least insofar as
;; nonnegative integers are concerned) by implementing 0 and the
;; operation of adding 1 as

(define zero-     (lambda (g) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

;; redefining for my own sanity:

(define identity  (lambda (x) x))
(define (zero g)  (lambda (g) identity))

;; This representation is known as "Church numerals", after its
;; inventor, Alonzo Church, the logician who invented the [lambda]
;; calculus.
;; 
;; Define `one' and `two' directly (not in terms of `zero' and
;; `add-1'). (Hint: Use substitution to evaluate `(add-1 zero)'). Give
;; a direct definition of the addition procedure `+' (not in terms of
;; repeated application of `add-1').

(define z zero)
(define s add-1)

;;; one
(add-1 zero)
(s z)
(lambda (f) (define n (lambda (g) (lambda (y) y))) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x))))
(lambda (f) (lambda (x) (f (((lambda (g) identity) f) x))))
(lambda (f) (lambda (x) (f (identity x))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;;; two
(s (s z))
;; substitute (s z) with one
(s one)

(lambda (f) (define n one) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g y))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (y) (f y)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; Give
;; a direct definition of the addition procedure `+' (not in terms of
;; repeated application of `add-1').

;; For addition what we want to do is to take eg
;;
;; (+ (f (f x))
;;    (f (f x)))
;;
;; and wind up with:
;;
;; (f (f (f (f x))))
;;
;; really:
;;
;; (+ (lambda (f) (lambda (x) (f (f x))))
;;    (lambda (f) (lambda (x) (f (f x)))))
;;
;; should be:
;;
;; (lambda (f) (lambda (x) (f (f (f (f x))))))
;;
;; I think all we need to do is take the one part out of add-1
;; 
;; (define one       (lambda (f) (lambda (x) (f x))))
;;                                       one ^^^^^
;;
;; (define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
;;                                             n ^^^^^
;;                                       one ^^^^^^^^^^^^^
;;
;; AH. To be clear; n isn't n... (n f) is n. likewise f isn't 1, the
;; wrapping is. And the fact that n is on the rhs is important too,
;; since we're playing with lambdas... so all my ideas to try to
;; substitute the rhs in as an arg to the lhs is wrong. I think.

(let ((a (lambda (g) (lambda (y) (g (g y)))))
      (b (lambda (h) (lambda (z) (h (h z))))))
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; I _think_

;; expanding the let above

(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g (g y)))) f)
                         (((lambda (h) (lambda (z) (h (h z)))) f) x))))
;; rhs simplify
(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g (g y)))) f)
                         ((lambda (z) (f (f z))) x))))
(lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g (g y)))) f)
                         (f (f x)))))
;; lhs simplify
(lambda (f) (lambda (x) ((lambda (y) (f (f y))) (f (f x)))))
(lambda (f) (lambda (x) (f (f (f (f x))))))

;; YAY!

(define (+ a b) (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; to test, substitute one in for a and reduce:
;; (I'm also substituting define -> lambda because mzscheme is annoying

(lambda (b)
  (let ((a one))
    (lambda (f) (lambda (x) ((a f) ((b f) x))))))

(lambda (b)
  (lambda (f) (lambda (x) ((one f) ((b f) x)))))

(lambda (b)
  (lambda (f) (lambda (x) (((lambda (g) (lambda (y) (g y))) f) ((b f) x)))))

(lambda (b)
  (lambda (f) (lambda (x) ((lambda (y) (f y)) ((b f) x)))))

(lambda (b)
  (lambda (f) (lambda (x) (f ((b f) x)))))

;; add-1:
(lambda (n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
