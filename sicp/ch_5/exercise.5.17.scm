#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 5.17

;; Extend the instruction tracing of *Note Exercise 5-16:: so that
;; before printing an instruction, the simulator prints any labels
;; that immediately precede that instruction in the controller
;; sequence. Be careful to do this in a way that does not interfere
;; with instruction counting (*Note Exercise 5-15::). You will have to
;; make the simulator retain the necessary label information.
