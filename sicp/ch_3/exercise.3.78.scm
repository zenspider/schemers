#lang racket/base

;;; Exercise 3.78

;; Consider the problem of designing a
;; signal-processing system to study the homogeneous second-order
;; linear differential equation
;;
;;      d^2 y        d y
;;      -----  -  a -----  -  by  =  0
;;      d t^2        d t
;;
;; The output stream, modeling y, is generated by a network that
;; contains a loop. This is because the value of d^2y/dt^2 depends
;; upon the values of y and dy/dt and both of these are determined by
;; integrating d^2y/dt^2.  The diagram we would like to encode is
;; shown in *Note Figure 3-35::.  Write a procedure `solve-2nd' that
;; takes as arguments the constants a, b, and dt and the initial
;; values y_0 and dy_0 for y and dy/dt and generates the stream of
;; successive values of y.
