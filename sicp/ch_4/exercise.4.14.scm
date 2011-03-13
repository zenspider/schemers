#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.14

;; Eva Lu Ator and Louis Reasoner are each
;; experimenting with the metacircular evaluator.  Eva types in the
;; definition of `map', and runs some test programs that use it.
;; They work fine.  Louis, in contrast, has installed the system
;; version of `map' as a primitive for the metacircular evaluator.
;; When he tries it, things go terribly wrong.  Explain why Louis's
;; `map' fails even though Eva's works.
