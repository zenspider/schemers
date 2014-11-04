#lang racket/base

(require (for-syntax racket/base))
(provide test assert-equal)

(define-syntax assert-equal
  (syntax-rules ()
    ((_ exp act)
     (module+ test
       (require rackunit)
       (require rackunit/text-ui)
       (check-equal? act exp)))))

(define-syntax test (make-rename-transformer #'assert-equal))
