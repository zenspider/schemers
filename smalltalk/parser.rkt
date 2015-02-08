#lang ragg

parse: expression | statements

;; method: message-pat (temporaries)? (statements)?
;; message-pat: unary-sel | binary-sel var-name | (keyword var-name)+
;; temporaries: "\|" (var-name)* "\|"
;; block: "[" ((":" var-name)+ "\|")? (statements)? "]"

statements: ( expression "." )* ( ("^")? expression )?

expression: (variable-name "_")* (primary | message-expression | cascade-expression)

cascade-expression: message-expression (cascade-extension)+
cascade-extension: ";" (unary-selector | binary-selector unary-object | (keyword binary-object)+ )

message-expression: unary-expression | binary-expression | keyword-expression

keyword-expression: binary-object (keyword binary-object)+

binary-expression: binary-object binary-selector unary-object

unary-expression: unary-object unary-selector

binary-object: unary-object | binary-expression

unary-object: primary | unary-expression

primary: variable-name | literal ;; TODO: | block ;; TODO: | "(" expression ")"

keyword: identifier ":"

binary-selector: BINARY_SEL

unary-selector: identifier

variable-name: identifier

literal: number | symbol-const | character | string | array-const

array-const: "#" array

array: "(" (number | symbol | string | character | array)* ")"

symbol-const: "#" symbol

symbol:     identifier | binary-selector | (keyword)+

number:     NUM
binary-op:  BIN
identifier: IDENT
character:  CHAR
string:     STR
