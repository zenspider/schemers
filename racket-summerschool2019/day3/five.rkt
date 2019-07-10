#lang racket

(require syntax/parse/define)

(provide define-five)

(define-simple-macro (define-five k:id v:expr)
  (begin
    (printf "Assuming that ~s produces 5~n" 'v)
    (define k 5)))
