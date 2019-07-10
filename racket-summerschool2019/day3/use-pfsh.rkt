#lang s-exp "pfsh.rkt"

(define out (ls "-1" -l))
out
42
(wc -l < out)
ls
