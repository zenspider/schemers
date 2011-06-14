#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.50

;; Use the compiler to compile the metacircular evaluator of section
;; *Note 4-1:: and run this program using the register-machine
;; simulator. (To compile more than one definition at a time, you can
;; package the definitions in a `begin'.) The resulting interpreter
;; will run very slowly because of the multiple levels of
;; interpretation, but getting all the details to work is an
;; instructive exercise.
