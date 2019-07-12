#lang racket

(require (for-syntax (prefix-in r: racket)
                     racket/pretty
                     syntax/parse
                     "stlc.rkt"))

(define-syntax (dbg/1 stx)
  (syntax-parse stx
    [(_ e)
     (r:#%app pretty-print (r:#%app syntax->datum (r:#%app expand-once #'e)))
     #'(void)]))

(define-syntax (dbg stx)
  (syntax-parse stx
    [(_ e)
     (r:#%app pretty-print (r:#%app syntax->datum (r:#%app expand #'e)))
     #'(void)]))

(dbg   (let ([a 10]) (+ a 1)))

;; outputs:
#;
'(#%app (lambda (a) (#%app +- a '1)) '10)

(dbg/1 (let ([a 10]) (+ a 1)))

;; outputs:
#;
'(erased
  ((lambda- (a) (erased (#%app- +- a (erased (#%datum- . 1)))))
   (erased (#%datum- . 10))))
