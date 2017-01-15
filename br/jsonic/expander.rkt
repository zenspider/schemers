#lang br/quicklang

(provide (rename-out [js-module-begin #%module-begin])
         json-char
         jsonic-program
         s-exp)

(require json)

(define-macro (js-module-begin PARSE-TREE)
  #'(#%module-begin
     (define result-string PARSE-TREE)
     (define validated-jsexpr (string->jsexpr result-string))
     (display result-string)))

(define-macro (json-char CHAR-STR)
  #'CHAR-STR)

(define-macro (jsonic-program SEXP-OR-JSON-STR ...)
  #'(string-trim (string-append SEXP-OR-JSON-STR ...)))

(define-macro (s-exp SEXP-STR)
  (with-pattern ([SEXP-DATUM (format-datum '~a #'SEXP-STR)])
    #'(jsexpr->string SEXP-DATUM)))
