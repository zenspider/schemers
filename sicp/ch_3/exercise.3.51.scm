#lang racket/base

(require "../lib/streams.scm")

;;; Exercise 3.51

;; In order to take a closer look at delayed evaluation, we will use
;; the following procedure, which simply returns its argument after
;; printing it:

(define (show x)
  (printf "~S~N" x)
  x)

;; What does the interpreter print in response to evaluating each
;; expression in the following sequence?(7)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(newline)
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 5

(stream-ref x 7)
;; 6
;; 7
;; 7
