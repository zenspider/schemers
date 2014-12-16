#lang racket/base

(provide smalltalk/lex)

(require ragg/support)                       ; token
(require parser-tools/lex)                   ; lexer-src-pos
(require (prefix-in : parser-tools/lex-sre)) ; regexps

(define-lex-abbrev ws (:* whitespace))
(define-lex-abbrev any any-string)
(define-lex-abbrev key-chars1 (char-set "/<>%&?,+=@-\\*~"))
(define-lex-abbrev key-chars2 (char-set "/<>%&?,+=@-\\*~!|"))

(define smalltalk/lex
  (lexer-src-pos
   [(eof) (void)]
   [(:or #\tab #\space #\newline #\page)       (smalltalk/lex input-port)]
   [(:: #\" (complement (:: any #\" any)) #\") (smalltalk/lex input-port)]

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
   ;; var_name    = identifier
   ;; unary_sel   = identifier
   ;; binary_sel  = "-" | special (special)?
   ;; keyword     = identifier ":"
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


   [(:: key-chars1 (:? key-chars2))                   (token 'BIN  lexeme)]
   [(:: alphabetic (:+ (:or alphabetic numeric)) ":") (token 'KEY  lexeme)]
   [(:: alphabetic (:* (:or alphabetic numeric)))     (token 'ID   lexeme)]
   [(:: "$" (:~ whitespace))                          (token 'CHAR lexeme)]
   [(:: "'" (complement (:: any "'" any)) "'")        (token 'STR  lexeme)]
   [(:+ numeric)                                      (token 'NUM  lexeme)]

   ;; semi-hack to get around ragg being a bitch
   [(:: "<primitive:" ws (:+ numeric) ws ">")             (token 'PRIM lexeme)]

   ["!"  (token lexeme lexeme)]
   ["#"  (token lexeme lexeme)]
   ["("  (token lexeme lexeme)]
   [")"  (token lexeme lexeme)]
   ["."  (token lexeme lexeme)]
   [":"  (token lexeme lexeme)]
   [":=" (token lexeme lexeme)]
   [";"  (token lexeme lexeme)]
   ["["  (token lexeme lexeme)]
   ["]"  (token lexeme lexeme)]
   ["^"  (token lexeme lexeme)]
   ["_"  (token lexeme lexeme)]
   ["|"  (token lexeme lexeme)]
   ))

(module+ test
  (require rackunit)
  (check-equal? (smalltalk/lex (open-input-string "2 + 3"))
                (position-token (token-struct 'NUM "2" #f #f #f #f #f)
                                (position 1 #f #f)
                                (position 2 #f #f))))
