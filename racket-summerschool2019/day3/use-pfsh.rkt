#lang s-exp "pfsh.rkt"

(define out (run ls "-1"))
out
(run wc -l < out)
