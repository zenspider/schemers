#lang racket/base

(provide smalltalk/lex)

(require ragg/support)                       ; token
(require parser-tools/lex)                   ; lexer-src-pos
(require (prefix-in : parser-tools/lex-sre)) ; regexps

(define-lex-abbrev digit   numeric)
(define-lex-abbrev digits  (:+ numeric))
(define-lex-abbrev number  (:: (:? digits "r") (:? "-") digits (:? "." digits) (:? "e" (:? "-") digits)))
(define-lex-abbrev letter  (:or (char-range "a" "z") (char-range "A" "Z")))
(define-lex-abbrev special (char-set "+/\\*~<>=@%|&?!"))
(define-lex-abbrev char    (:or digit letter special (char-set "[]{}()_^,;$!#:")))
(define-lex-abbrev ident   (:: letter (:* (:or letter digit))))

(define-lex-abbrev any any-string)
;; (define-lex-abbrev ws (:* whitespace))
;; (define-lex-abbrev key-chars1 (char-set "/<>%&?,+=@-\\*~"))
;; (define-lex-abbrev key-chars2 (char-set "/<>%&?,+=@-\\*~!|"))

(define-lex-abbrev binary-sel (:or "-" (:: special (:? special))))
;; (define-lex-abbrev keyword (:: ident ":"))

(define-syntax-rule (skip) (return-without-pos (smalltalk/lex input-port)))
(define-syntax-rule (same) (token lexeme lexeme))
(define-syntax-rule (end)  (return-without-pos eof))
(define-syntax-rule (tok k)  (token k lexeme))

(define smalltalk/lex
  (lexer-src-pos

   ;; skippables
   [(eof)                                      (end)]
   [(:or #\tab #\space #\newline)              (skip)]
   [(:: #\" (complement (:: any #\" any)) #\") (skip)]

   ;; [(:: alphabetic (:+ (:or alphabetic numeric)) ":") (tok 'KEY)]
   ;; [(:: "$" (:~ whitespace))                          (tok 'CHAR)]
   ;; [(:: "'" (complement (:: any "'" any)) "'")        (tok 'STR)]
   ;; [(:: key-chars1 (:? key-chars2))                   (tok 'BIN)]
   ;; [(:: alphabetic (:* (:or alphabetic numeric)))     (tok 'ID)]

   [number                                            (tok 'NUM)]
   ;; ident       = letter (letter|digit)*
   [ident (tok 'IDENT)]

   ;; sym_const   = "#" symbol
   ;; [(:: "#" (:or ident binary-sel (:+ keyword))) (tok 'SYM_CONST)]
   [(:: binary-sel) (tok 'BINARY_SEL)]

   ;; char_const  = "$" (char | "'" | "\"")
   [(:: "$" (:or char "'" "\"")) (tok 'CHAR)]

   ;; string      = "'" (char | "'" "'" | "\"")* "'"
   [(:: "'" (:* (:or char (:: "'" "'") "\"")) "'") (tok 'STR)]

   ;; ;; semi-hack to get around ragg being a bitch
   ;; [(:: "<primitive:" ws (:+ numeric) ws ">")         (tok 'PRIM)]
   ;;
   ;; ["!"  (same)]
   ["#"  (same)]
   ["("  (same)]                        ; TODO: not sure if this is the right way
   [")"  (same)]
   ["."  (same)]
   [":"  (same)]
   ;; [":=" (same)]
   [";"  (same)]
   ;; ["["  (same)]
   ;; ["]"  (same)]
   ;; ["^"  (same)]
   ["_"  (same)]
   ;; ["|"  (same)]
   ))
