#lang racket

(provide run)

(define (run prog . args)
  (apply system* (find-program prog) args))

(define (find-program str)
  (or (find-executable-path str)
      (error 'pfsh "could not find program: ~a" str)))
