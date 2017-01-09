#lang brag

bf-program : (op | loop)*
op         : ">" | "<" | "+" | "-" | "." | ","
loop       : "[" (op | loop)* "]"
