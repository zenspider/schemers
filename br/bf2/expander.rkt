#lang br/quicklang

(provide (rename-out [bf-module-begin #%module-begin])
         bf-program
         loop
         op)

(define-macro (bf-module-begin EXPR)
  #'(#%module-begin EXPR))

(define (fold-funcs acc fns)
  (for/fold ([acc acc])
            ([fn (in-list fns)])
    (apply fn acc)))

(define-macro (bf-program OP/LOOP ...)
  #'(begin
      (define state (list (make-vector 30000 0) 0))
      (void (fold-funcs state (list OP/LOOP ...)))))

(define-macro (loop "[" OP/LOOP ... "]")
  #'(lambda (arr ptr)
      (for/fold ([acc (list arr ptr)])
                ([i (in-naturals)]
                 #:break (zero? (apply current-byte acc)))
        (fold-funcs acc (list OP/LOOP ...)))))

(define-macro-cases op
  [(op ">") #'gt]
  [(op "<") #'lt]
  [(op "+") #'plus]
  [(op "-") #'minus]
  [(op ".") #'period]
  [(op ",") #'comma])

(define current-byte      vector-ref)
(define (set-current-byte arr ptr val)
  (vector-set! arr ptr val)
  arr)

(define (gt arr ptr)     (list arr (add1 ptr)))
(define (lt arr ptr)     (list arr (sub1 ptr)))

(define (plus arr ptr)
  (list (set-current-byte arr ptr (add1 (current-byte arr ptr)))
        ptr))

(define (minus arr ptr)
  (list (set-current-byte arr ptr (sub1 (current-byte arr ptr)))
        ptr))

(define (period arr ptr)
  (write-byte (current-byte arr ptr))
  (list arr ptr))
(define (comma arr ptr)
  (list (set-current-byte arr ptr (read-byte)) ptr))
