#lang racket

(provide (all-defined-out))

(require RacketSchool/private/mystery-records
         RacketSchool/private/basic
         racket/function
         racket/gui/base
         racket/list
         racket/match
         racket/port
         racket/string)

(define mine   'RecImpl)
(define theirs 'RacketSchool/Records1)

(define ns (make-gui-namespace))

(define (clean s)
  (string-trim s #px"\\s+|'"))

(define (run-with-lang port lang)
  (define input (input-port-append #t   ; prepend a #lang line
                                   (open-input-string (format "#lang ~a~n" lang))
                                   port))
  (read-accept-reader #t)
  (port-count-lines! input)             ; TODO: verify the line nos are correct
  (define syntax (read-syntax (object-name input) input))

  (define this (make-resolved-module-path (gensym lang)))
  (parameterize ([current-module-declare-name this]
                 [current-namespace ns])
    (with-handlers ([exn:fail?
                     (λ (e) (list 'error (exn-message e)
                                  (rest (fourth (syntax->datum syntax)))))])
      (eval syntax ns)
      (clean
       (with-output-to-string
         (thunk
          (dynamic-require this #f)))))))

(define (run-with-langs src lang1 lang2)
  (define s1 (run-with-lang (open-input-string src) lang1))
  (define s2 (run-with-lang (open-input-string src) lang2))
  (list s1 s2))

(define (compare src lang1 lang2)
  (match-define `(,s1 ,s2) (run-with-langs src lang1 lang2))
  ;; (printf "mine=~v~ntheirs=~v~n" s1 s2)
  (if (equal? "stuck" s2)
      (if (list? s1)
          'both-stuck
          (begin (printf "false positive: ~a~n  vs ~a~n" s1 s2)
                 #f))
      (if (equal? s1 s2)
          #t
          (begin (printf "diff: ~a~n  vs ~a~n" s1 s2)
                 #f))))

(define (sexp->string sexp)
  (with-output-to-string (thunk (write sexp))))
(define (sexp->port sexp)
  (open-input-string (sexp->string sexp)))
(define (string->port str)
  (open-input-string str))

(module+ main
  (require redex/reduction-semantics)

  ;; (random-seed 42)

  (redex-check record-lang-1 e
               (begin (printf "e=~v~n" (sexp->string (term e)))
                      (compare (sexp->string (term e)) mine theirs))))

(module+ test
  (define src "true")
  (compare src mine theirs))
