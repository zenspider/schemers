#lang racket/base

;;; Exercise 3.82

;; Redo *Note Exercise 3-5:: on Monte Carlo
;; integration in terms of streams.  The stream version of
;; `estimate-integral' will not have an argument telling how many
;; trials to perform.  Instead, it will produce a stream of estimates
;; based on successively more trials.
