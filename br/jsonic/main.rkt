#lang br/quicklang

(module reader br
  (require "reader.rkt")
  (provide read-syntax)
  (define (get-info port mod line col pso)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'jsonic/colorer 'color-jsonic)]
        [(drracket:indentation)
         (dynamic-require 'jsonic/indenter 'indent-jsonic)]
        [(drracket:toolbar-buttons)
         (dynamic-require 'jsonic/buttons 'button-list)]
        [else default]))
    handle-query))
