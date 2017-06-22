#lang racket/base

(provide (all-defined-out))

(struct end-program-signal ())
(struct change-line-signal (val))
(struct line-error (msg))

(define (raise-line-error str)
  (raise (line-error str)))

(define (handle-line-error num le)
  (error (format "error  in line ~a: ~a" num (line-error-msg le))))
