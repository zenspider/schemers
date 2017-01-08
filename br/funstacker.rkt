#lang br/quicklang

(provide read-syntax
         handle
         (rename-out [funstacker-module-begin #%module-begin])
         + *
         )

(define-macro (funstacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (first stack)))

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (datum->syntax #f `(module funstacker-mod "funstacker.rkt"
                       ,@src-datums)))

(define (handle [arg #f])
  (cond [(number? arg)
         (push-stack! arg)]
        [(or (equal? + arg)
             (equal? * arg))
         (define result (arg (pop-stack) (pop-stack)))
         (push-stack! result)]
        ))

(define stack empty)

(define (pop-stack)
  (define item (first stack))
  (set! stack (rest stack))
  item)

(define (push-stack! item)
  (set! stack (cons item stack)))

(module+ test
  (require rackunit)
  (require syntax/macro-testing)

  (check-equal? (syntax->datum
                 (read-syntax 'path (open-input-string "4\n8\n+\n3\n*\n")))
                '(module funstacker-mod "funstacker.rkt"
                   (handle 4)
                   (handle 8)
                   (handle +)
                   (handle 3)
                   (handle *)))

  (displayln "done"))
