#lang br/quicklang

(provide #%module-begin wire AND OR NOT LSHIFT RSHIFT)

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define wire-datums
    (for/list ([wire-str (in-lines port)])
      (format-datum '(wire ~a) wire-str)))
  (strip-bindings
   #`(module wires-mod wires/main
       #,@wire-datums)))

(define-macro-cases wire
  [(wire)                    #'(void)]
  [(wire ARG -> ID)          #'(define/display (ID) (val ARG))]
  [(wire OP ARG -> ID)       #'(define/display (ID) (OP (val ARG)))]
  [(wire ARG1 OP ARG2 -> ID) #'(define/display (ID) (OP (val ARG1) (val ARG2)))])

(define-macro (define/display (ID) BODY)
  #'(begin
      (define (ID) BODY)
      (module+ main
        (displayln (format "~a: ~a" 'ID (ID))))))

(define val
  (let ([val-cache (make-hash)])
    (lambda (num-or-wire)
      (if (number? num-or-wire)
          num-or-wire
          (hash-ref! val-cache num-or-wire num-or-wire)))))

(define (mod-16bit x) (modulo x #x10000))
(define-macro (define-16bit ID PROC-ID)
  #'(define ID (compose1 mod-16bit PROC-ID)))

(define-16bit AND bitwise-and)
(define-16bit OR bitwise-ior)
(define-16bit NOT bitwise-not)
(define-16bit LSHIFT arithmetic-shift)
(define (RSHIFT x y) (LSHIFT x (- y)))

(module+ test
  (require rackunit)

  (check-equal? (test-reader read-syntax "123 AND g -> ac")
                '(module wires-mod wires/main
                   (wire 123 AND g -> ac)))

  (displayln 'done))
