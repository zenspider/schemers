#lang br/quicklang

(provide (rename-out [b-module-begin #%module-begin])
         (matching-identifiers-out #rx"^b-" (all-defined-out)))

(struct end-program-signal ())
(struct change-line-signal (val))

(define (run line-table)
  (define line-vec (list->vector (sort (hash-keys line-table) <)))
  (with-handlers ([end-program-signal? (lambda (exn-val) (void))])
    (for/fold ([line-idx 0])
              ([i (in-naturals)]
               #:break (>= line-idx (vector-length line-vec)))
      (define line-num (vector-ref line-vec line-idx))
      (define line-func (hash-ref line-table line-num))
      (with-handlers
        ([change-line-signal?
          (lambda (cls)
            (define clsv (change-line-signal-val cls))
            (or (and (exact-positive-integer? clsv)
                     (vector-member clsv line-vec))
                (error (format "error in line ~a: line ~a not found"
                               line-num clsv))))])
        (line-func)
        (add1 line-idx)))))

(define-macro (b-module-begin (b-program LINE ...))  ; TODO: rename b-program?
  (with-pattern
    ([((b-line NUM STATEMENT ...) ...) #'(LINE ...)]
     [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))])
    #'(#%module-begin
       LINE ...
       (define line-table (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (void (run line-table)))))

(define-macro (b-line NUM STATEMENT ...)
  (with-pattern ([LINE-NUM (prefix-id "line-" #'NUM
                                      #:source #'NUM)])
    (syntax/loc caller-stx
      (define (LINE-NUM) (void) STATEMENT ...))))

(define (b-rem val) (void))

(define (b-end) (raise (end-program-signal)))

(define (b-print . vals)
  (displayln (string-append* (map ~a vals))))

(define (b-goto expr) (raise (change-line-signal expr)))

(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

(define (b-sum . nums) (apply + nums))
