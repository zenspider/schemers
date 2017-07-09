#lang br

(provide basic-output-port do-setup!)

(define basic-output-port (make-parameter (open-output-nowhere)))

(define (do-setup!)
  (basic-output-port (current-output-port)))
