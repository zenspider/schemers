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
