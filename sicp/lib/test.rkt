#lang racket/base

(require (for-syntax racket/base))
(provide assert-equal
         assert-many
         optional                       ; move
         define-me-maybe null atom? list?
         assert-float assert-float-d test/error       ; deprecate
         refute xassert done test test-eq test-group) ; deprecate

(require rackunit)
(require rackunit/log)

(executable-yield-handler (lambda (_) (test-log #:exit? #t)))

(define (done)
  (void))

(define-syntax optional                 ; TODO: move me
  (syntax-rules ()
    ((_ var val)
     (or (and (not (null? var)) (car var)) val))))

(define-syntax (define-me-maybe stx)
  (syntax-case stx ()
    ((_ (name . args) body ...)
     #'(define-me-maybe name (lambda args body ...)))
    ((_ name value)
     (if (identifier-binding #'name)
         #'(begin) #'(define name value)))))

(define-syntax test/error
  (syntax-rules ()
    ((_ body ...)
     (check-exn exn:fail? (lambda () body ...)))))

(define-syntax test-group
  (syntax-rules ()
    ((_ name body ...)
     (test-case name body ...))))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ exp act)
     (check-equal? act exp))))

(define (assert-float-d x y d)
  (xassert (< (abs (- x y)) d)))

(define (assert-float x y)
  (assert-float-d x y 0.00001))

(define-syntax refute
  (syntax-rules ()
    ((_ exp) (if (not exp) (display ".")
                 (begin
                   (display "F: (refute ")
                   (write (quote exp))
                   (display ")")
                   (newline)
                   (display ";; => ")
                   (display "(refute ")
                   (write exp)
                   (display ")")
                   (newline))))))

(define-syntax xassert
  (syntax-rules ()
    ((_ exp) (if exp (display ".")
                 (begin
                   (display "F: (xassert ")
                   (write (quote exp))
                   (display ")")
                   (newline)
                   (display ";; => ")
                   (display "(xassert ")
                   (write exp)
                   (display ")")
                   (newline))))))

(define (assert-many tests . futs)
  (for-each tests futs))

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
