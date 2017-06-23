#lang racket/base

(require (for-syntax br/syntax
                     racket/base)
         br/define
         racket/format
         racket/string
         "struct.rkt")

(provide (all-defined-out))

(define (bool->int val) (if val 1 0))
(define nonzero? (compose not zero?))

(define-macro (b-line NUM STATEMENT ...)
  (with-pattern ([LINE-NUM (prefix-id "line-" #'NUM
                                      #:source #'NUM)])
    (syntax/loc caller-stx
      (define (LINE-NUM #:error [msg #f])
        (with-handlers ([line-error? (lambda (le) (handle-line-error NUM le))])
          (when msg (raise-line-error msg))
          STATEMENT ...)))))

(define (b-rem val) (void))

(define (b-end) (raise (end-program-signal)))

(define (b-print . vals)
  (displayln (string-append* (map ~a vals))))

(define (b-goto expr) (raise (change-line-signal expr)))

(define-macro (b-let ID VAL) #'(set! ID VAL))

(define-macro-cases b-if
  [(_ COND-EXPR TRUE-EXPR) #'(b-if COND-EXPR TRUE-EXPR (void))]
  [(_ COND-EXPR TRUE-EXPR FALSE-EXPR)
   #'(let ([result (if (nonzero? COND-EXPR) TRUE-EXPR FALSE-EXPR)])
       (when (exact-positive-integer? result)
         (b-goto result)))])

(define-macro (b-input ID)
  #'(b-let ID (let* ([str (read-line)]
                     [num (string->number (string-trim str))])
                (or num str))))

(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

(define-macro-cases b-or-expr
  [(_ VAL) #'VAL]
  [(_ LHS "or" RHS) #'(bool->int (or (nonzero? LHS) (nonzero? RHS)))])

(define-macro-cases b-and-expr
  [(_ VAL) #'VAL]
  [(_ LHS "and" RHS) #'(bool->int (and (nonzero? LHS) (nonzero? RHS)))])

(define-macro-cases b-not-expr
  [(_ VAL) #'VAL]
  [(_ "not" RHS) #'(bool->int (not (nonzero? RHS)))])

(define b=  (compose1 bool->int =))
(define b<  (compose1 bool->int <))
(define b>  (compose1 bool->int >))
(define b<> (compose1 bool->int not =))

(define-macro-cases b-comp-expr
  [(_ VAL) #'VAL]
  [(_ LHS "=" RHS)  #'(b=  LHS RHS)]
  [(_ LHS "<" RHS)  #'(b<  LHS RHS)]
  [(_ LHS ">" RHS)  #'(b>  LHS RHS)]
  [(_ LHS "<>" RHS) #'(b<> LHS RHS)])

(define-macro-cases b-sum
  [(_ VAL) #'VAL]
  [(_ LHS "+" RHS) #'(+ LHS RHS)]
  [(_ LHS "-" RHS) #'(- LHS RHS)])

(define-macro-cases b-product
  [(_ VAL) #'VAL]
  [(_ LHS "*" RHS) #'(* LHS RHS)]
  [(_ LHS "/" RHS) #'(/ LHS RHS 1.0)]
  [(_ LHS "mod" RHS) #'(modulo LHS RHS)])

(define-macro-cases b-neg
  [(_ VAL) #'VAL]
  [(_ "-" VAL) #'(- VAL)])

(define-macro-cases b-expt
  [(_ VAL) #'VAL]
  [(_ LHS "^" RHS) #'(expt LHS RHS)])
