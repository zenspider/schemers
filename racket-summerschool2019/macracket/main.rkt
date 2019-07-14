#lang racket/base

(require reprovide/reprovide)

(reprovide (for-syntax racket/base syntax/parse)
           racket/base
           racket/function
           racket/list
           syntax/parse/define)

(module reader syntax/module-reader
  macracket)
