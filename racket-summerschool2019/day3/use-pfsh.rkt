#lang s-exp "pfsh.rkt"

(define out (run ls -l))

out
out

(run cd /Users/ryan)

(define l (run ls))
