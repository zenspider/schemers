#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.28

;; `Eval' uses `actual-value' rather than `eval' to
;; evaluate the operator before passing it to `apply', in order to
;; force the value of the operator.  Give an example that
;; demonstrates the need for this forcing.
