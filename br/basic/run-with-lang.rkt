#lang racket/base

(require racket/port)

(provide run-with-lang)

(define ns (make-base-namespace))

(define (run-with-lang port lang)
  (define input (input-port-append #t   ; prepend a #lang line
                                   (open-input-string (format "#lang ~a~n" lang))
                                   port))
  (read-accept-reader #t)
  (port-count-lines! input)             ; TODO: verify the line nos are correct
  (define syntax (read-syntax (object-name input) input))

  (define this (make-resolved-module-path (gensym 'basic)))
  (current-module-declare-name this)
  (eval syntax ns)

  (parameterize ([current-namespace ns])
    (dynamic-require this #f)))
