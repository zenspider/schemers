#lang brag

 b-program   : [b-line] (/NEWLINE [b-line])*
 b-line      : b-line-num [b-statement] (/":" b-statement)* [b-rem]
@b-line-num  : INTEGER
@b-statement : b-end | b-print | b-goto | b-let | b-input | b-if
             | b-gosub | b-return | b-for | b-next
             | b-def
             | b-import
             | b-export
 b-rem       : REM
 b-end       : /"end"
 b-print     : /"print" [b-printable] (/";" [b-printable])*
@b-printable : STRING | b-expr
 b-goto      : /"goto" b-expr
 b-let       : [/"let"] b-id /"=" (b-expr | STRING)
 b-if        : /"if" b-expr /"then" (b-statement | b-expr)
                           [/"else" (b-statement | b-expr)]
 b-input     : /"input" b-id
@b-id        : ID
 b-gosub     : /"gosub" b-expr
 b-return    : /"return"
 b-for       : /"for" b-id /"=" b-expr /"to" b-expr [/"step" b-expr]
 b-next      : /"next" b-id
 b-def       : /"def" b-id /"(" b-id [/"," b-id]* /")" /"=" b-expr
 b-import    : /"import" b-import-nm
@b-import-nm : RACKET-ID | STRING
 b-export    : /"export" b-export-nm
@b-export-nm : ID
 b-expr      : b-or-expr
 b-or-expr   : [b-or-expr "or"] b-and-expr
 b-and-expr  : [b-and-expr "and"] b-not-expr
 b-not-expr  : ["not"] b-comp-expr
 b-comp-expr : [b-comp-expr ("=" | "<" | ">" | "<>")] b-sum
 b-sum       : [b-sum ("+"|"-")] b-product
 b-product   : [b-product ("*"|"/"|"mod")] b-neg
 b-neg       : ["-"] b-expt
 b-expt      : [b-expt "^"] b-value
@b-value     : b-number | b-id | /"(" b-expr /")" | b-func
 b-func      : (ID | RACKET-ID) /"(" b-expr [/"," b-expr]* /")"
@b-number    : INTEGER | DECIMAL
