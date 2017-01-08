#lang br/quicklang

(provide read-syntax
         handle-args
         (rename-out [funstacker-module-begin #%module-begin])
         + *
         )

(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     (first HANDLE-ARGS-EXPR)))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (datum->syntax #f `(module funstacker-mod "funstacker.rkt"
                       (handle-args ,@src-datums))))

(define (handle-args . args)
  (for/fold ([stack-acc empty])
            ([arg (in-list args)]
             #:unless (void? arg))
    (cond
      [(number? arg) (cons arg stack-acc)]
      [(or (equal? * arg) (equal? + arg))
       (define op-result
         (arg (first stack-acc) (second stack-acc)))
       (cons op-result (drop stack-acc 2))])))

(module+ test
  (require rackunit)
  (require syntax/macro-testing)

  (check-equal? (syntax->datum
                 (read-syntax 'path (open-input-string "\n\n4\n8\n+\n3\n*\n")))
                `(module funstacker-mod "funstacker.rkt"
                   (handle-args ,(void) ,(void) 4 8 + 3 *)))

  (displayln "done"))
