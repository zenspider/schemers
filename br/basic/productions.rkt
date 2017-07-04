#lang racket/base

(require (for-syntax br/syntax
                     racket/base)
         br/define
         racket/format
         racket/string
         racket/list
         br/list
         "struct.rkt")

(provide (all-defined-out))

(define (bool->int val) (if val 1 0))
(define nonzero? (compose not zero?))
(define return-ccs empty)

(define (in-closed-interval? x start end)
  ((if (< start end) <= >=) start x end))

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

(define (b-gosub num-expr)
  (let/cc here-cc
    (push! return-ccs here-cc)
    (b-goto num-expr)))

(define (b-return)
  (unless (not (empty? return-ccs)) ; TODO: unless not -> when
    (raise-line-error "return without gosub"))
  (define top-cc (pop! return-ccs))
  (top-cc (void)))

(define next-funcs (make-hasheq))

(define-macro-cases b-for
  [(_ LOOP-ID START END) #'(b-for LOOP-ID START END 1)]
  [(_ LOOP-ID START END STEP)
   #'(b-let LOOP-ID
            (let/cc loop-cc
              (hash-set! next-funcs
                         'LOOP-ID
                         (lambda ()
                           (define next-val (+ LOOP-ID STEP))
                           (if (in-closed-interval? next-val START END)
                               (loop-cc next-val)
                               (hash-remove! next-funcs 'LOOP-ID))))
              START))])

(define-macro (b-next LOOP-ID)
  #'(begin
      (unless (hash-has-key? next-funcs 'LOOP-ID)
        (raise-line-error (format "`next ~a` without for in ~a" 'LOOP-ID (hash-keys next-funcs))))
      (define func (hash-ref next-funcs 'LOOP-ID))
      (func)))

(define-macro (b-def FUNC-ID VAR-ID ... EXPR)
  (syntax-local-lift-expression
   #'(set! FUNC-ID (lambda (VAR-ID ...) EXPR))))

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

(define-macro (b-func FUNC-ID ARG ...)
  #'(if (procedure? FUNC-ID)
        (FUNC-ID ARG ...)
        (raise-line-error (format "expected ~a to be a function, got ~v"
                                  'FUNC-ID FUNC-ID))))
