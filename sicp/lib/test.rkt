#lang racket/base

(require (for-syntax racket/base))
(provide assert-equal atom? define-me-maybe
         test test-eq test-group)       ; deprecate

(require rackunit)

(define-syntax define-me-maybe
  (lambda (stx)
    (syntax-case stx ()
      ((_ name value)
       (if (identifier-binding #'name)
           #'(begin) #'(define name value)))
      ((_ (name . args) body ...)
       #'(define-me-maybe name (lambda args body ...))))))

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

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))
