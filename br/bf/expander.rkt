#lang br/quicklang

(provide (rename-out [bf-module-begin #%module-begin])
         bf-program
         loop
         op)

(define-macro (bf-module-begin EXPR)
  #'(#%module-begin EXPR))

(define-macro (bf-program OP/LOOP ...)
  #'(void OP/LOOP ...))

(define-macro (loop "[" OP/LOOP ... "]")
  #'(until (zero? (current-byte))
           OP/LOOP ...))

(define-macro-cases op
  [(op ">") #'(gt)]
  [(op "<") #'(lt)]
  [(op "+") #'(plus)]
  [(op "-") #'(minus)]
  [(op ".") #'(period)]
  [(op ",") #'(comma)])

(define arr (make-vector 30000 0))
(define ptr 0)

(define (current-byte)          (vector-ref  arr ptr))
(define (set-current-byte! val) (vector-set! arr ptr val))

(define (gt)     (set! ptr (add1 ptr)))
(define (lt)     (set! ptr (sub1 ptr)))
(define (plus)   (set-current-byte! (add1 (current-byte))))
(define (minus)  (set-current-byte! (sub1 (current-byte))))
(define (period) (write-byte (current-byte)))
(define (comma)  (set-current-byte! (read-byte)))
