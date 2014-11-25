#lang racket/base

(require (for-syntax racket/base))
(provide assert-equal
         define-me-maybe null atom? list?
         test test-eq test-group)       ; deprecate

(require rackunit)

(define-syntax (define-me-maybe stx)
  (syntax-case stx ()
    ((_ (name . args) body ...)
     #'(define-me-maybe name (lambda args body ...)))
    ((_ name value)
     (if (identifier-binding #'name)
         #'(begin) #'(define name value)))))

(define-syntax test-group
  (syntax-rules ()
    ((_ name body ...)
     (test-case name body ...))))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ exp act)
     (check-equal? act exp))))

(define-syntax test-eq
  (syntax-rules ()
    ((_ exp act)
     (check-equal? act exp))))

(define-syntax test (make-rename-transformer #'assert-equal))

(define-me-maybe null '())

(define-me-maybe (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define-me-maybe (list? x)
  (not (atom? x)))
