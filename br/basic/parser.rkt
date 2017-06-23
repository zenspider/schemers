#lang brag

 b-program   : [b-line] (/NEWLINE [b-line])*
 b-line      : b-line-num [b-statement] (/":" b-statement)* [b-rem]
@b-line-num  : INTEGER
@b-statement : b-end | b-print | b-goto | b-let | b-input
 b-rem       : REM
 b-end       : /"end"
 b-print     : /"print" [b-printable] (/";" [b-printable])*
@b-printable : STRING | b-expr
 b-goto      : /"goto" b-expr
 b-let       : [/"let"] b-id /"=" (b-expr | STRING)
 b-input     : /"input" b-id
@b-id        : ID
 b-expr      : b-sum
 b-sum       : [b-sum ("+"|"-")] b-product
 b-product   : [b-product ("*"|"/"|"mod")] b-neg
 b-neg       : ["-"] b-expt
 b-expt      : [b-expt "^"] b-value
@b-value     : b-number | b-id | /"(" b-expr /")"
@b-number    : INTEGER | DECIMAL
