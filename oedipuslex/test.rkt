#lang oedipuslex

digits = (:+ numeric)

(eof)                                                 : (return-without-srcloc eof)
"\n"                                                  : (token 'NEWLINE lexeme)
whitespace                                            : (token lexeme #:skip? #t)
(:or "print" "goto" "end" "+" ":" ";")                : (token lexeme lexeme)
digits                                                : (token 'INTEGER (string->number lexeme))
(:or (:seq (:? digits) "." digits) (:seq digits ".")) : (token 'DECIMAL (string->number lexeme))
(:or (from/to "\"" "\"") (from/to "'"  "'"))          : (token 'STRING (string-trim lexeme #px"."))

;; TODO: phase 3: maybe?
;; (eof)                                      : (return-without-srcloc eof)
;; "\n"                                       : (token 'NEWLINE lexeme)
;; whitespace                                 : (token lexeme #:skip? #t)
;; "rem" ... "\n"                             : (token 'REM lexeme)
;; "print" | "goto" | "end" | "+" | ":" | ";" : (token lexeme lexeme)
;; digits                                     : (token 'INTEGER (string->number lexeme))
;; digits? "." digits | digits "."            : (token 'DECIMAL (string->number lexeme))
;;  "\"" .. "\"" | "'" .. "'"                 : (token 'STRING (string-trim lexeme #px"."))
