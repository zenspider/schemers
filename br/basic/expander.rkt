#lang br/quicklang

(require "struct.rkt" "run.rkt" "productions.rkt"
         "setup.rkt")

(provide (rename-out [b-module-begin #%module-begin])
         (all-from-out "productions.rkt"))

(define-macro (b-module-begin (b-program LINE ...))  ; TODO: rename b-program?
  (with-pattern
    ([((b-line NUM STATEMENT ...) ...) #'(LINE ...)]
     [(LINE-FUNC ...)   (prefix-id "line-" #'(NUM ...))]
     [(VAR-ID ...)      (find-property 'b-id #'(LINE ...))]
     [(IMPORT-NAME ...) (find-property 'b-import-nm #'(LINE ...))]
     [(EXPORT-NAME ...) (find-property 'b-export-nm #'(LINE ...))]
     )
    #'(#%module-begin
       (module configure-runtime br
         (require basic/setup)
         (do-setup!))
       (require IMPORT-NAME ...)
       (provide EXPORT-NAME ...)
       (define VAR-ID 0) ...
       LINE ...
       (define line-table (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (parameterize ([current-output-port (basic-output-port)])
         (void (run line-table))))))

(begin-for-syntax
  (require racket/list)
  (define (find-property which line-stxs)
    (remove-duplicates
     (for/list ([stx (in-list (stx-flatten line-stxs))]
                #:when (syntax-property stx which))
       stx)
     #:key syntax->datum)))
