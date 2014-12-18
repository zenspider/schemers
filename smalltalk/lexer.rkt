#lang racket/base

(provide smalltalk/lex)

(require ragg/support)                       ; token
(require parser-tools/lex)                   ; lexer-src-pos
(require (prefix-in : parser-tools/lex-sre)) ; regexps

;; From the Blue Book. Consider this a TODO:
;;
;; digit       = [0-9]
;; digits      = (digit)+
;; number      = (digits "r")? ("-")? digits ("." digits)? ("e" ("-")? digits)?
;; letter      = [A-Za-z]
;; special     = [+/\*~<>=@%|&?!]
;; char        = digit | lettter | special | [\[\]{}()_^,;$!#:]
;; ident       = letter (letter|digit)*
;; symbol      = ident | binary_sel | (keyword)+
;; sym_const   = "#" symbol
;; char_const  = "$" (char | "'" | "\"")
;; string      = "'" (char | "'" "'" | "\"")* "'"
;; comment     = "\"" (char | "\"" "\"" | "'")* "\""
;; array       = "(" (number | symbol | string | char_const | array)* ")"
;; array_const = "#" array
;; literal     = number | sym_const | char_const | string | array_const
;; var_name    = ident
;; unary_sel   = ident
;; binary_sel  = "-" | special (special)?
;; keyword     = ident ":"
;; primary     = var_name | literal | block | "(" expression ")"
;; unary_obj   = primary | unary_exp
;; binary_obj  = unary_obj | binary_exp
;; unary_exp   = unary_obj unary_sel
;; binary_exp  = binary_obj binary_sel unary_obj
;; keyword_exp = binary_obj (keyword binary_obj)+
;; message_exp = unary_exp | binary_exp | keyword_exp
;; cascade_exp = message_exp (";" unary_sel | binary_sel unary_obj | (keyword binary_obj)+ )+
;; expr        = (var_name "_")? (primary | message_exp | cascade_exp)
;; statements  = ( | "^" expr | (expr "." statements) ) # FIX

;; block = "[" ((":" var_name)+ "|")? (statements)? "]"
;; temporaries = "|" (var_name)* "|"
;; message_pat = unary_sel | binary_sel var_name | (keyword var_name)+
;; method      = message_pat (temporaries)? (statements)?

(define-lex-abbrev ws (:* whitespace))
(define-lex-abbrev any any-string)
(define-lex-abbrev key-chars1 (char-set "/<>%&?,+=@-\\*~"))
(define-lex-abbrev key-chars2 (char-set "/<>%&?,+=@-\\*~!|"))

(define-syntax-rule (skip) (return-without-pos (smalltalk/lex input-port)))
(define-syntax-rule (same) (token lexeme lexeme))

(define smalltalk/lex
  (lexer-src-pos
   [(eof)                                      (return-without-pos eof)]
   [(:or #\tab #\space #\newline #\page)       (skip)]
   [(:: #\" (complement (:: any #\" any)) #\") (skip)]

   [(:: key-chars1 (:? key-chars2))                   (token 'BIN  lexeme)]
   [(:: alphabetic (:+ (:or alphabetic numeric)) ":") (token 'KEY  lexeme)]
   [(:: alphabetic (:* (:or alphabetic numeric)))     (token 'ID   lexeme)]
   [(:: "$" (:~ whitespace))                          (token 'CHAR lexeme)]
   [(:: "'" (complement (:: any "'" any)) "'")        (token 'STR  lexeme)]
   [(:+ numeric)                                      (token 'NUM  lexeme)]

   ;; semi-hack to get around ragg being a bitch
   [(:: "<primitive:" ws (:+ numeric) ws ">")             (token 'PRIM lexeme)]

   ["!"  (same)]
   ["#"  (same)]
   ["("  (same)]
   [")"  (same)]
   ["."  (same)]
   [":"  (same)]
   [":=" (same)]
   [";"  (same)]
   ["["  (same)]
   ["]"  (same)]
   ["^"  (same)]
   ["_"  (same)]
   ["|"  (same)]
   ))

(module+ test
  (require rackunit
           racket/sequence
           check-sexp-equal)

  (define (drain-lexer input)
    (define ip (open-input-string input))
    (sequence->list (in-producer smalltalk/lex eof ip)))

  (define (check-lex input . expecteds)
    (define actuals (drain-lexer input))
    (if (= (length actuals) (length expecteds))
        (for ([actual actuals] [expected expecteds])
          (check-sexp-equal? actual expected))
        (check-sexp-equal? actuals expecteds)))

  (define (tk t v m n)
    (position-token (token-struct t v #f #f #f #f #f)
                    (position m #f #f) (position n #f #f)))

  (check-lex "2 + 3"
             (tk 'NUM "2" 1 2)
             (tk 'BIN "+" 3 4)
             (tk 'NUM "3" 5 6)))
