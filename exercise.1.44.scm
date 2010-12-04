#lang racket

;;; Exercise 1.44:

;; The idea of "smoothing" a function is an important concept in
;; signal processing. If f is a function and dx is some small number,
;; then the smoothed version of f is the function whose value at a
;; point x is the average of f(x - dx), f(x), and f(x + dx). Write a
;; procedure `smooth' that takes as input a procedure that computes f
;; and returns a procedure that computes the smoothed f. It is
;; sometimes valuable to repeatedly smooth a function (that is, smooth
;; the smoothed function, and so on) to obtained the "n-fold smoothed
;; function". Show how to generate the n-fold smoothed function of any
;; given function using `smooth' and `repeated' from *Note Exercise
;; 1-43::.

;; TODO