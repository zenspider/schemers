#lang racket

(provide assert assert-equal assert-many done)

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
    ((_ exp act) (if (equal? exp act)
                     (display ".")
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


(define (assert-many tests . futs)
  (for-each tests futs))

(define (done)
  (display "done!")
  (newline))
