#lang racket

(require redex)
(require racket/gui/base)

(define mine   'RecImpl)
(define theirs 'RacketSchool/Records1)

;; (random-seed 42)

(define ns (make-gui-namespace))

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

(define (clean s)
  (string-trim s)
  (string-trim                          ; newlines
   (string-trim s "'" #:right? #f))     ; literal ticks on left
)

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

(require RacketSchool/private/basic)
(require RacketSchool/private/mystery-records)

(define generator (generate-term record-lang-1 e))

(define (sexp->string sexp)
  (with-output-to-string (thunk (write sexp))))
(define (sexp->port sexp)
  (open-input-string (sexp->string sexp)))
(define (string->port str)
  (open-input-string str))

(define src "true")
(compare src mine theirs)

(redex-check record-lang-1 e
             (begin (printf "e=~v~n" (sexp->string (term e)))
                    (compare (sexp->string (term e)) mine theirs))
             #:attempts 25)

;; TODO: write the file out and run it and capture the output

;; (define *iter* 100)
;;
;; 'read+eval
;; (time
;;  (for ([i (in-range *iter*)])
;;    (eval (read (open-input-string "(+ 1 2)")) ns)))
;;
;; 'racket/base
;; (time
;;  (for ([i (in-range *iter*)])
;;    (run-with-lang (open-input-string "(+ 1 2)") 'racket/base)))
;;
;; 'racket
;; (time
;;  (for ([i (in-range *iter*)])
;;    (run-with-lang (open-input-string "(+ 1 2)") 'racket)))
;;
;; mine
;; (time
;;  (for ([i (in-range *iter*)])
;;    (run-with-lang (open-input-string "(+ 1 2)") mine)))
;;
;; theirs
;; (time
;;  (for ([i (in-range *iter*)])
;;    (run-with-lang (open-input-string "(+ 1 2)") theirs)))
