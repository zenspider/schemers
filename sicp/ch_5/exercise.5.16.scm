#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.16

;; Augment the simulator to provide for "instruction tracing". That
;; is, before each instruction is executed, the simulator should print
;; the text of the instruction. Make the machine model accept
;; `trace-on' and `trace-off' messages to turn tracing on and off.
