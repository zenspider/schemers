#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.23

;; Extend the evaluator to handle derived
;; expressions such as `cond', `let', and so on (section *Note
;; 4-1-2::).  You may "cheat" and assume that the syntax transformers
;; such as `cond->if' are available as machine operations.(1)
