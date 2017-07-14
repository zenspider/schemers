#lang racket

(require redex)

(provide (all-defined-out))

;; syntax

(define-language basic-syntax
  (p ::= (prog f ... e))
  (f ::= (defun (x x) e))
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (zero? e)
     (+ e e)
     ;; strings
     s
     (empty? e)
     (++ e e)
     ;; functions & let
     (function x)
     (e e)
     x
     (let ((x e)) e))
  (x ::= variable-not-otherwise-mentioned)
  (b ::= true false)
  (n ::= number)
  (s ::= string)
  (v ::=
     b
     n
     s
     (function x))
  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))

(default-language basic-syntax)

;; evaluation

(define-extended-language basic-lang basic-syntax
  (P ::= (prog f ... E))
  (E ::=
     hole
     ;; booleans
     (if E e e)
     ;; numbers
     (zero? E)
     (+ E e)
     (+ v E)
     ;; strings
     (empty? E)
     (++ E e)
     (++ v E)
     ;; functions & let
     (E e)
     (v E)
     (let ((x E)) e)))

(define basic->
  (reduction-relation basic-lang
    ;; booleans
    (--> (in-hole P (if true e_then e_else))
         (in-hole P e_then)
         e-if-true)
    (--> (in-hole P (if false e_then e_else))
         (in-hole P e_else)
         e-if-false)
    ;; numbers
    (--> (in-hole P (zero? 0))
         (in-hole P true)
         e-zero-yes)
    (--> (in-hole P (zero? n))
         (in-hole P false)
         (side-condition (not (equal? (term n) 0)))
         e-zero-no)
    (--> (in-hole P (+ n_1 n_2))
         (in-hole P ,(+ (term n_1) (term n_2)))
         e-plus)
    ;; strings
    (--> (in-hole P (empty? ""))
         (in-hole P true)
         e-empty-yes)
    (--> (in-hole P (empty? s))
         (in-hole P false)
         (side-condition (not (equal? (term s) "")))
         e-empty-no)
    (--> (in-hole P (++ s_1 s_2))
         (in-hole P ,(string-append (term s_1) (term s_2)))
         e-append)
    ;; termination
    (--> (prog f ... v)
         v
         e-halt)
    ;; id
    (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
               (in-hole E x_fun))
         (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
               (in-hole E (function x_fun)))
         e-id)
    ;; let
    (--> (in-hole P (let ((x v)) e))
         (in-hole P (substitute e x v))
         e-let)
    ;; apply
    (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
               (in-hole E ((function x_fun) v_arg)))
         (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
               (in-hole E (substitute e_body x_param v_arg)))
         e-apply)))

(define-extended-language basic-lang-2 basic-lang
  ;; nothing yet
  )

(default-language basic-lang-2)


(define basic->2
  (extend-reduction-relation
   basic-> basic-lang
   ;; nothing yet
   ))



(test--> basic->
    (term (prog
           (defun (f x)
             (+ x 1))
           (+ 1 2)))
    42)
