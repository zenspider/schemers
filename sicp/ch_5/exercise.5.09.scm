#!/usr/bin/env csi -s

(require rackunit)
(require-library machine)
(import machine)

;;; Exercise 5.9

;; The treatment of machine operations above permits them to operate
;; on labels as well as on constants and the contents of registers.
;; Modify the expression-processing procedures to enforce the
;; condition that operations can be used only with registers and
;; constants.

#;(test-error
 (make-machine
  '(a)
  (list (list '+ +))
  '(a
    (perform (op +) (label a)))))
