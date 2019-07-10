#lang racket/base

(require syntax/parse/define)

(provide thunk-time-it)

(define (thunk-time-it thunk)
  (time (thunk)))
