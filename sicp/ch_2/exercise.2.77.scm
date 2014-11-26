(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.77

;; Louis Reasoner tries to evaluate the expression
;; `(magnitude z)' where `z' is the object shown in *Note Figure
;; 2-24::.  To his surprise, instead of the answer 5 he gets an error
;; message from `apply-generic', saying there is no method for the
;; operation `magnitude' on the types `(complex)'.  He shows this
;; interaction to Alyssa P. Hacker, who says "The problem is that the
;; complex-number selectors were never defined for `complex' numbers,
;; just for `polar' and `rectangular' numbers.  All you have to do to
;; make this work is add the following to the `complex' package:"
;;
;;      (put 'real-part '(complex) real-part)
;;      (put 'imag-part '(complex) imag-part)
;;      (put 'magnitude '(complex) magnitude)
;;      (put 'angle '(complex) angle)
;;
;; Describe in detail why this works.

;; A: Calling the puts above attaches the 4 "methods" for the complex
;;    type. It gets the complex accessors by virtue of the fact that
;;    you used make-from-real-imag which dispatches to the proper
;;    rectangular constructor. This is, in essence, smalltalk's double
;;    dispatch design pattern.

;; As an example, trace through all the procedures called in
;; evaluating the expression `(magnitude z)' where `z' is the object
;; shown in *Note Figure 2-24::. In particular, how many times is
;; `apply-generic' invoked? What procedure is dispatched to in each
;; case?

;; (define (apply-generic op arg) (arg op))
;; (define (magnitude z) (apply-generic 'magnitude z))
;;
;; (define z (make-complex-from-real-imag 3 4))
;; -> (lambda-thingy '(complex rectangular 3 . 4))

;; (magnitude z)
;; (apply-generic 'magnitude z))
;; (z 'magnitude)
;; ((get z '(complex) 'magnitude) z)
;; (sqrt (+ (square (real-part z))
;;          (square (imag-part z))))
;; ... and so on
