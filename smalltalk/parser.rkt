#lang ragg

parse: (classDefinition "!")+ ["!"]

classDefinition: classHeader (method "!")*
                 | (staticStatement)+

staticStatement:                        ; this sucks. rewrite
        className ( keyword
                    "#" className
                    keyword string
                    keyword string
                    keyword string
                    keyword (identifier | string)
                  | keyword string )
                  ["."]

classHeader: "!" className [identifier] keyword string "!"

method: messagePattern [temporaries] [primitive] statements

primitive: PRIM

messagePattern: unarySelector | binarySelector var | (keyword var)+

temporaries: "|" (var)* "|"

;; bad translation :(
;; statements: (expression (expression ".")*)? ("^" expression (".")?)?

statements: [nonEmptyStatements]

nonEmptyStatements:
        "^" expression ["."]
      | expression ["." statements]

expression:
        [var assign] var assign expression
      | simpleExpression

simpleExpression: primary [messageExpression (";" messageElt)*]

messageElt:
        unarySelector
      | binarySelector unaryObjectDescription
      | (keyword binaryObjectDescription)+

messageExpression:
        unaryExpression
      | binaryExpression
      | keywordExpression

unaryExpression: (unarySelector)+ [binaryExpression | keywordExpression]

binaryExpression: (binarySelector unaryObjectDescription)+ [keywordExpression]

keywordExpression: (keyword binaryObjectDescription)+

unaryObjectDescription: primary (unarySelector)*

binaryObjectDescription:
        primary (unarySelector)* (binarySelector unaryObjectDescription)*

primary: literal | var | block | "(" expression ")"

literal: number | character | string | "#" (symbol|array)

block: "[" [(":" var)+ "|"] statements "]"

array: "(" (arrayElt)* ")"

arrayElt: number | character | string | symbol | array

symbol: identifier | binarySelector | keyword

unarySelector: identifier

binarySelector: binary-op | "|"

type: "(" className ")"

className:  identifier
var:        identifier
assign:     (":=" | "_")
binary-op:  BIN
keyword:    KEY
identifier: ID
character:  CHAR
string:     STR
number:     NUM
