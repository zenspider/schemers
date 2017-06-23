#lang brag

 b-program   : [b-line] (/NEWLINE [b-line])*
 b-line      : b-line-num [b-statement] (/":" b-statement)* [b-rem]
@b-line-num  : INTEGER
@b-statement : b-end | b-print | b-goto | b-let | b-input | b-if
             | b-gosub | b-return | b-for | b-next
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
 b-expr      : b-or-expr
 b-or-expr   : [b-or-expr "or"] b-and-expr
 b-and-expr  : [b-and-expr "and"] b-not-expr
 b-not-expr  : ["not"] b-comp-expr
 b-comp-expr : [b-comp-expr ("=" | "<" | ">" | "<>")] b-sum
 b-sum       : [b-sum ("+"|"-")] b-product
 b-product   : [b-product ("*"|"/"|"mod")] b-neg
 b-neg       : ["-"] b-expt
 b-expt      : [b-expt "^"] b-value
@b-value     : b-number | b-id | /"(" b-expr /")"
@b-number    : INTEGER | DECIMAL
