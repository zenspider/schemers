#lang at-exp

;; (define basic-lexer                     ; ORIGINAL
;;   (lexer-srcloc
;;    [(eof) (return-without-srcloc eof)]
;;    ["\n"  (token 'NEWLINE lexeme)]
;;    [whitespace (token lexeme #:skip? #t)]
;;    [(from/stop-before "rem" "\n") (token 'REM lexeme)]
;;    [(:or "print" "goto" "end" "+" ":" ";") (token lexeme lexeme)]
;;    [digits (token 'INTEGER (string->number lexeme))]
;;    [(:or (:seq (:? digits) "." digits)
;;          (:seq digits "."))
;;     (token 'DECIMAL (string->number lexeme))]
;;    [(:or (from/to "\"" "\"")
;;          (from/to "'"  "'"))
;;     (token 'STRING (string-trim lexeme #px"."))]))

;; phase 1: just strip define+lexer-srcloc
[(eof)                                                 (return-without-srcloc eof)]
["\n"                                                  (token 'NEWLINE lexeme)]
[whitespace                                            (token lexeme #:skip? #t)]
[(from/stop-before "rem" "\n")                         (token 'REM lexeme)]
[(:or "print" "goto" "end" "+" ":" ";")                (token lexeme lexeme)]
[digits                                                (token 'INTEGER (string->number lexeme))]
[(:or (:seq (:? digits) "." digits) (:seq digits ".")) (token 'DECIMAL (string->number lexeme))]
[(:or (from/to "\"" "\"") (from/to "'"  "'"))          (token 'STRING (string-trim lexeme #px"."))]


;; (eof)                                                 : (return-without-srcloc eof)
;; "\n"                                                  : (token 'NEWLINE lexeme)
;; whitespace                                            : (token lexeme #:skip? #t)
;; (from/stop-before "rem" "\n")                         : (token 'REM lexeme)
;; (:or "print" "goto" "end" "+" ":" ";")                : (token lexeme lexeme)
;; digits                                                : (token 'INTEGER (string->number lexeme))
;; (:or (:seq (:? digits) "." digits) (:seq digits ".")) : (token 'DECIMAL (string->number lexeme))
;; (:or (from/to "\"" "\"") (from/to "'"  "'"))          : (token 'STRING (string-trim lexeme #px"."))

;; (eof)                                      : (return-without-srcloc eof)
;; "\n"                                       : (token 'NEWLINE lexeme)
;; whitespace                                 : (token lexeme #:skip? #t)
;; "rem" ... "\n"                             : (token 'REM lexeme)
;; "print" | "goto" | "end" | "+" | ":" | ";" : (token lexeme lexeme)
;; digits                                     : (token 'INTEGER (string->number lexeme))
;; digits? "." digits | digits "."            : (token 'DECIMAL (string->number lexeme))
;;  "\"" .. "\"" | "'" .. "'"                 : (token 'STRING (string-trim lexeme #px"."))
