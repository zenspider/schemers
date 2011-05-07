#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.33

;; Consider the following definition of a factorial
;; procedure, which is slightly different from the one given above:
;;
;;      (define (factorial-alt n)
;;        (if (= n 1)
;;            1
;;            (* n (factorial-alt (- n 1)))))
;;
;; Compile this procedure and compare the resulting code with that
;; produced for `factorial'.  Explain any differences you find.  Does
;; either program execute more efficiently than the other?
