
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)
;; (require "../lib/table.rkt")

;;; Exercise 2.73

;; Section *Note 2-3-2:: described a program that
;; performs symbolic differentiation:
;;
;;      (define (deriv exp var)
;;        (cond ((number? exp) 0)
;;              ((variable? exp) (if (same-variable? exp var) 1 0))
;;              ((sum? exp)
;;               (make-sum (deriv (addend exp) var)
;;                         (deriv (augend exp) var)))
;;              ((product? exp)
;;               (make-sum
;;                 (make-product (multiplier exp)
;;                               (deriv (multiplicand exp) var))
;;                 (make-product (deriv (multiplier exp) var)
;;                               (multiplicand exp))))
;;              <MORE RULES CAN BE ADDED HERE>
;;              (else (error "unknown expression type -- DERIV" exp))))
;;
;; We can regard this program as performing a dispatch on the type of
;; the expression to be differentiated.  In this situation the "type
;; tag" of the datum is the algebraic operator symbol (such as `+')
;; and the operation being performed is `deriv'.  We can transform
;; this program into data-directed style by rewriting the basic
;; derivative procedure as

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         (else ((get 'deriv (operator exp)) (operands exp)
;;                var))))
;;
;; (define (operator exp) (car exp))
;;
;; (define (operands exp) (cdr exp))

;;   a. Explain what was done above.  Why can't we assimilate the
;;      predicates `number?' and `same-variable?' into the
;;      data-directed dispatch?

;; A: number? and same-variable? aren't datatype specific so it
;;    doesn't make sense to have them as an operator on deriv

;;   b. Write the procedures for derivatives of sums and products,
;;      and the auxiliary code required to install them in the table
;;      used by the program above.

;; (define deriv-table (make-table))
;; (define get (deriv-table 'lookup-proc))
;; (define put (deriv-table 'insert-proc!))
;;
;; (put 'deriv '+ (lambda (operands var)
;;                  (make-sum (deriv (addend exp) var)
;;                            (deriv (augend exp) var))))
;;
;; (put 'deriv '* (lambda (operands var)
;;                  (make-sum
;;                   (make-product (multiplier exp)
;;                                 (deriv (multiplicand exp) var))
;;                   (make-product (deriv (multiplier exp) var)
;;                                 (multiplicand exp)))))

;;   c. Choose any additional differentiation rule that you like,
;;      such as the one for exponents (*Note Exercise 2-56::), and
;;      install it in this data-directed system.

;; (put 'deriv '** (lambda (operands var)
;;                   (make-product
;;                    (make-product
;;                     (exponent exp)
;;                     (make-exponentiation (base exp)
;;                                          (make-sum (exponent exp) -1)))
;;                    (deriv (base exp) var))))

;;   d. In this simple algebraic manipulator the type of an
;;      expression is the algebraic operator that binds it together.
;;      Suppose, however, we indexed the procedures in the opposite
;;      way, so that the dispatch line in `deriv' looked like
;;
;;           ((get (operator exp) 'deriv) (operands exp) var)
;;
;;      What corresponding changes to the derivative system are
;;      required?

;; A: none. you just have to have your put's reversed as well.
