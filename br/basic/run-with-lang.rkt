#lang racket/base

(require racket/port
         racket/string
         racket/function)

(provide run-with-lang)

(define (run-with-lang port lang)
  (define input (input-port-append #t   ; prepend a #lang line
                                   (open-input-string (format "#lang ~a~n" lang))
                                   port))
  (read-accept-reader #t)
  (port-count-lines! input)             ; TODO: verify the line nos are correct
  (define syntax (read-syntax (object-name input) input))

  (define ns (make-base-namespace))

  (define this (make-resolved-module-path 'a-module))
  (parameterize ([current-module-declare-name this]
                 [current-namespace ns])
    (eval syntax ns)
    (string-trim (with-output-to-string
                   (thunk (dynamic-require this #f))))))

(module+ test
  (require rackunit
           rackunit/log)

  (define (go src)
    (run-with-lang (open-input-string src) 'racket))

  (check-equal? (go "(+ 1 2 3)") "6")

  (void (test-log #:display? #t)))
