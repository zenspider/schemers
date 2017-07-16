#lang racket

(require "driver.rkt")
(require benchmark-ips)

(define ns2 (make-base-namespace))

(define (shell-run-with-lang src lang)
  (with-output-to-file "blah.rkt" #:exists 'truncate/replace
    (thunk
     (displayln (format "#lang ~a~n" lang))
     (displayln src)))
  (clean (with-output-to-string (thunk (system "racket blah.rkt")))))

(benchmark/ips
 'read+eval1
 (eval (read (open-input-string "(+ 1 2)")) ns)

 'read+eval2
 (eval (read (open-input-string "(+ 1 2)")) ns2))

(benchmark/ips
 'racket/base
 (run-with-lang (open-input-string "(+ 1 2)") 'racket/base)

 'racket
 (run-with-lang (open-input-string "(+ 1 2)") 'racket)

 'mine
 (run-with-lang (open-input-string "(+ 1 2)") mine)

 'theirs
 (run-with-lang (open-input-string "(+ 1 2)") theirs)

 'file+racket/base
 (shell-run-with-lang '(+ 1 2) 'racket/base)

 'file+racket
 (shell-run-with-lang '(+ 1 2) 'racket))
