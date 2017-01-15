#lang br

(require jsonic/parser
         jsonic/tokenizer
         brag/support)

(parse-tree (apply-tokenizer tokenize "// line commment\n"))
(parse-tree (apply-tokenizer tokenize "@$ 42 $@"))
(parse-tree (apply-tokenizer tokenize "hi"))
(parse-tree (apply-tokenizer tokenize "hi\n// comment\n@$ 42 $@"))
(parse-tree (apply-tokenizer tokenize
                             #<<SHECKY
"foo"
// comment
@$ 42 $@
SHECKY
                             ))
