#lang racket

(provide assert assert-equal done)

;; (define (assert test)
;;   (if test (display ".")
;;       (error "failed")))

; print-test : a macro for printing tests.
(define-syntax assert
  (syntax-rules ()
    ((_ exp) (if test (display ".")
                 (begin
                   (display "F: (assert ")
                   (write (quote exp))
                   (display ")")
                   (newline)
                   (display ";; => ")
                   (display "(assert ")
                   (write exp)
                   (display ")")
                   (newline))))))

; print-test : a macro for printing tests.
(define-syntax assert-equal
  (syntax-rules ()
    ((_ exp act) (when (not (equal? exp act))
                   (begin
                     (display "F: (assert-equal ")
                     (write (quote exp))
                     (display " ")
                     (write (quote act))
                     (display ")")
                     (newline)
                     (display ";; => ")
                     (display "(assert-equal ")
                     (write exp)
                     (display " ")
                     (write act)
                     (display ")")
                     (newline))))))

(define (done)
  (display "done!")
  (newline))
