#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define)

(module+ test
  (require rackunit))

(define-simple-macro (trace e)
  (trace/d e 0))

#;(
   e ::=
   id
   number
   (id e ...)
   )

(define-syntax (trace/d stx)
  (syntax-parse stx
    [(_ n:number d:nat)          #'n]
    [(_ x:id d:nat)              #'x]
    [(_ (f:id e:expr ...) d:nat)
     (define d+1 (add1 (syntax-e #'d)))
     #`(print-and-return d '(f e ...) (f (trace/d e #,d+1) ...))]))

(define (print-and-return depth expr val)
  (printf "~a~s = ~s\n"
          (make-string (* 2 depth) #\space)
          expr
          val)
  val)

(module+ test
  (define-simple-check (check-output? fn expect)
    (check-equal? (with-output-to-string fn)
                  expect))

  (check-output? (thunk (trace pi))
                 "")

  (check-output? (thunk (trace 42))
                 "")

  (check-output? (thunk (trace (+ 1 2)))
                 "(+ 1 2) = 3\n")

  (check-output? (thunk (trace (+ (* 3 4) (/ 4 2))))
                 (string-append "  (* 3 4) = 12\n"
                                "  (/ 4 2) = 2\n"
                                "(+ (* 3 4) (/ 4 2)) = 14\n")))

#;(trace (+ (+ 1 2) (+ 3 4)))

(define-syntax five 5)
(define-syntax six 6)

(define-syntax (show-me stx)
  (syntax-parse stx
    [(_ x:id)
     (printf "~s = ~s\n" (syntax-e #'x) (syntax-local-value #'x))
     #'42]))

#;(show-me five)                        ; five = 5
#;(show-me six)                         ; six = 6

(module+ test
  (displayln 'done))
