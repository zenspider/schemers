(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.81

;; Louis Reasoner has noticed that `apply-generic'
;; may try to coerce the arguments to each other's type even if they
;; already have the same type.  Therefore, he reasons, we need to put
;; procedures in the coercion table to "coerce" arguments of each
;; type to their own type.  For example, in addition to the
;; `scheme-number->complex' coercion shown above, he would do:
;;
;;      (define (scheme-number->scheme-number n) n)
;;      (define (complex->complex z) z)
;;      (put-coercion 'scheme-number 'scheme-number
;;                    scheme-number->scheme-number)
;;      (put-coercion 'complex 'complex complex->complex)
;;
;;   a. With Louis's coercion procedures installed, what happens if
;;      `apply-generic' is called with two arguments of type
;;      `scheme-number' or two arguments of type `complex' for an
;;      operation that is not found in the table for those types?
;;      For example, assume that we've defined a generic
;;      exponentiation operation:
;;
;;           (define (exp x y) (apply-generic 'exp x y))
;;
;;      and have put a procedure for exponentiation in the
;;      Scheme-number package but not in any other package:
;;
;;           ;; following added to Scheme-number package
;;           (put 'exp '(scheme-number scheme-number)
;;                (lambda (x y) (tag (expt x y)))) ; using primitive `expt'
;;
;;      What happens if we call `exp' with two complex numbers as
;;      arguments?

;; A: Louis is an idiot. It'd infinitely recurse.

;;   b. Is Louis correct that something had to be done about coercion
;;      with arguments of the same type, or does `apply-generic' work
;;      correctly as is?

;; A: Of course not. It is Louis. apply-generic only coerces if proc
;; is null.

;;   c. Modify `apply-generic' so that it doesn't try coercion if the
;;      two arguments have the same type.

;; A: no. I'd rather modify put-coersion to error if the types are the
;;    same. I don't address symptoms. That said, I bet anyone can
;;    figure out how to put an if statement in apply-generic. Besides,
;;    it DOESN'T have the problem, so what's the point?

