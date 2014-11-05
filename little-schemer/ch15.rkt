#lang racket/base

(require "../sicp/lib/test.rkt")
(module+ test (require rackunit))

(define-me-maybe atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;; meh... I understand all of this

;;; The 16th Commandment
;; Use (set! ...) only with names defined in (let ...)s.

;;; The 17th Commandment (preliminary version)
;; Use (set! x ...) for (let ((x ...)) ...) only if there is at least
;; one (lambda ...) between it and the (let ((x ...)) ...).

;;; The 18th Commandment
;; Use (set! x ...) only when the value that x refers to is no longer
;; needed.
