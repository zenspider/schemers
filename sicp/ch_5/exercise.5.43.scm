#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.43

;; We argued in section *Note 4-1-6:: that internal
;; definitions for block structure should not be considered "real"
;; `define's.  Rather, a procedure body should be interpreted as if
;; the internal variables being defined were installed as ordinary
;; `lambda' variables initialized to their correct values using
;; `set!'.  Section *Note 4-1-6:: and *Note Exercise 4-16:: showed
;; how to modify the metacircular interpreter to accomplish this by
;; scanning out internal definitions.  Modify the compiler to perform
;; the same transformation before it compiles a procedure body.
