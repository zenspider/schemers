#lang racket/base

(require (for-syntax br/syntax
                     racket/base)
         br/define
         racket/format
         racket/string
         "struct.rkt")

(provide (all-defined-out))

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

(define-macro (b-input ID)
  #'(b-let ID (let* ([str (read-line)]
                     [num (string->number (string-trim str))])
                (or num str))))

(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

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
