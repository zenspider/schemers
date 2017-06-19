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



;; #lang brag
;;
;;  b-program   : [b-line] (/NEWLINE [b-line])*
;;  b-line      : b-line-num [b-statement] (/":" b-statement)* [b-rem]
;; @b-line-num  : INTEGER
;; @b-statement : b-end | b-print | b-goto
;;  b-rem       : REM
;;  b-end       : /"end"
;;  b-print     : /"print" [b-printable] (/";" [b-printable])*
;; @b-printable : STRING | b-expr
;;  b-goto      : /"goto" b-expr
;;  b-expr      : b-sum
;;  b-sum       : b-number (/"+" b-number)*
;; @b-number    : INTEGER | DECIMAL

(define-macro (b-module-begin (b-program LINE ...))  ; TODO: rename b-program?
  (with-pattern
    (
     [((b-line NUM STATEMENT ...) ...) #'(LINE ...)]
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

;; '(b-program
;;   (b-line 00 (b-rem "rem this is some text as a comment"))
;;   (b-line 10 (b-print "Hello") (b-print "world"))
;;   (b-line 20 (b-goto (b-expr (b-sum 9 10 11))))
;;   (b-line 30 (b-end)))

;; '(b-program
;;   (b-line 30 (b-rem "rem print 'ignored'"))
;;   (b-line 35)
;;   (b-line 50 (b-print "never gets here"))
;;   (b-line 40 (b-end))
;;   (b-line 60 (b-print "three") (b-print (b-expr (b-sum 1.0 3))))
;;   (b-line 70 (b-goto (b-expr (b-sum 11.0 18.5 0.5))))
;;   (b-line 10 (b-print "o" "n" "e"))
;;   (b-line 20 (b-print) (b-goto (b-expr (b-sum 60.0))) (b-end)))
