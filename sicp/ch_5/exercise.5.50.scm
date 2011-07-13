#!/usr/bin/env csi -s

(use test)
(require-library compiler)
(import compiler)

;;; Exercise 5.50

;; Use the compiler to compile the metacircular evaluator of section
;; *Note 4-1:: and run this program using the register-machine
;; simulator. (To compile more than one definition at a time, you can
;; package the definitions in a `begin'.) The resulting interpreter
;; will run very slowly because of the multiple levels of
;; interpretation, but getting all the details to work is an
;; instructive exercise.

;; TODO

(let ((exp '(cond ((a) 1)
                  ((b) 2)
                  (else 3))))
  (test (cdr exp) (cond-clauses exp))
  (test '(if (a) 1 (if (b) 2 3)) (cond->if exp)))

(define eval-sexp #f)
(with-input-from-file "lib/eval.scm"
  (lambda ()
    (set! eval-sexp (drop (read) 4))
    (set-car! eval-sexp 'begin)))

(append-eceval-operations
 'pair? pair?)

(assert-compile 42 eval-sexp)
