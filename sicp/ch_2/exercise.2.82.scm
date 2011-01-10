#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 2.82

;; Show how to generalize `apply-generic' to handle
;; coercion in the general case of multiple arguments.  One strategy
;; is to attempt to coerce all the arguments to the type of the first
;; argument, then to the type of the second argument, and so on.

;; this is a very naive implementation that only tries to coerce to
;; the first type. Recursion isn't hard, but since I can't even write
;; tests for this, I'd rather get more real work done and come back to
;; this later:

;; (define (apply-generic op . args)
;;   (define (coerce arg coersion) (if (null? coersion) arg (coersion arg)))
;;   (define (coercions type-tags)
;;     (let ((base (car type-tags))
;;           (rest (cdr type-tags)))
;;       (define (iterate l r)
;;         (if (null? l) r
;;             (let ((t (get-coersion base (car l))))
;;               (if (null? t)
;;                   (error "No coersion between types" (list base t))
;;                   (iterate (cdr l) (cons t r))))))
;;       (iterate rest '())))
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           ;; if I knew how to splat args in, it'd be like this:
;;           (apply-generic
;;            op
;;            (splat (map coerce (zip args (coersions type-tags)))))))))

;; Give an example of a situation where this strategy (and likewise
;; the two-argument version given above) is not sufficiently general.
;; (Hint: Consider the case where there are some suitable mixed-type
;; operations present in the table that will not be tried.)

;; A: well, the obvious case is where there is a helper arg that isn't
;;    in the heirarchy:
;;
;;      (apply-generic op lower higher bool)
;;
;;    then there is the case where coersion simply shouldn't happen.
;;    Eg, there is no reason why multiplying a rational by an integer
;;    needs to coerce the integer to a rational. Granted, it doesn't
;;    hurt, but there probably as many situations where it would cause
;;    problems as not.
;;
;;    we also haven't worked out multiple layer coersions, tho we've
;;    talked about it so I guess it is assumed to exist/work.
;;
;;    other than that, and assuming we're not working on a multiple
;;    inheritance model, I can't think of too many places where this
;;    wouldn't work.
