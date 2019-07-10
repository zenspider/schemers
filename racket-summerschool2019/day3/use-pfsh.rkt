#lang s-exp "pfsh.rkt"

(define out (run ls -l))
out
(run wc -l < out)
