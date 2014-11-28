#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 5.13

;; Modify the simulator so that it uses the controller sequence to
;; determine what registers the machine has rather than requiring a
;; list of registers as an argument to `make-machine'. Instead of
;; pre-allocating the registers in `make-machine', you can allocate
;; them one at a time when they are first seen during assembly of the
;; instructions.
