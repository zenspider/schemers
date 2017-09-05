#lang racket

(require syntax/parse/define)           ; define-simple-macro

(module+ test
  (require rackunit))

(define (emit fmt . args)
  (apply printf fmt args)
  (newline))

(define (asm x)
  (cond [(symbol? x) (format "%~a" x)]
        [(number? x) (format "$~a" x)]))

(define (pushq reg)    (emit "  pushq ~a"    (asm reg)))
(define (popq reg)     (emit "  popq ~a"     (asm reg)))
(define (movq from to) (emit "  movq ~a, ~a" (asm from) (asm to)))
(define (movl from to) (emit "  movl ~a, ~a" (asm from) (asm to)))
(define (retq)         (emit "  retq"))
(define (entry name)   (emit "_~a:"          name))

(define-simple-macro  (stack-wrap e ...)
  (begin
    (pushq 'rbp)
    (movq 'rsp 'rbp)
    e ...
    (popq 'rbp)))

(define (my-compile-program x)
  (entry 'scheme_entry)
  (stack-wrap
   (movl 42 'eax))
  (retq))

(define (compile-program x)
  (entry 'new_scheme_entry)
  (movl x 'eax)
  (retq)
  )

(my-compile-program 42)
(compile-program 42)

(define (cc c-src)
  (define (c [s ""]) (printf "~a~n" s))
  (with-output-to-file "compile.c" #:exists 'replace
    (thunk
     (c #<<C
#include <stdio.h>

int scheme_entry() {
C
      )

     (c (format "  ~a;" c-src))
     (c "}")
     (c)

     (c #<<C
int main(int argc, char** argv) {
  printf("%d\n", scheme_entry());
  return 0;
}
C
      )))
  (string-trim
   (with-output-to-string (λ () (system "cc compile.c && ./a.out")))))

(module+ test
  (check-equal? (cc "return 42")
                "42"))

(module+ test
  (displayln 'done))
