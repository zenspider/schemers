#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.72

;; Why do `disjoin' and `stream-flatmap' interleave
;; the streams rather than simply append them?  Give examples that
;; illustrate why interleaving works better.  (Hint: Why did we use
;; `interleave' in section *Note 3-5-3::?)
