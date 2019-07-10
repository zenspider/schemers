#lang racket

(provide run run-with-input)

(define (run prog . args)
  (apply system* (find-program prog) args))

(define (run-with-input input prog . args)
  (with-input-from-string input
    (lambda ()
      (apply run prog args))))

(define (find-program str)
  (or (find-executable-path str)
      (error 'pfsh "could not find program: ~a" str)))

;; (run-with-input "this is some input\n" "wc" "-l")
