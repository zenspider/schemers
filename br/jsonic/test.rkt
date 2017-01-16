#lang br

(require jsonic/parser
         jsonic/tokenizer
         brag/support)

(module+ test
  (require rackunit)

  (check-equal? (parse-tree (apply-tokenizer tokenize "// line commment\n"))
                '(jsonic-program))
  (check-equal? (parse-tree (apply-tokenizer tokenize "@$ 42 $@"))
                '(jsonic-program (s-exp " 42 ")))
  (check-equal? (parse-tree (apply-tokenizer tokenize "hi"))
                '(jsonic-program (json-char "h") (json-char "i")))
  (check-equal? (parse-tree (apply-tokenizer tokenize "hi\n// comment\n@$ 42 $@"))
                '(jsonic-program (json-char "h")
                                 (json-char "i")
                                 (json-char "\n")
                                 (s-exp " 42 ")))
  (check-equal? (parse-tree (apply-tokenizer tokenize
  #<<SHECKY
  "foo"
  // comment
  @$ 42 $@
SHECKY
  ))
                '(jsonic-program (json-char " ")
                                 (json-char " ")
                                 (json-char "\"")
                                 (json-char "f")
                                 (json-char "o")
                                 (json-char "o")
                                 (json-char "\"")
                                 (json-char "\n")
                                 (json-char " ")
                                 (json-char " ")
                                 (json-char " ")
                                 (json-char " ")
                                 (s-exp " 42 ")))

  (displayln 'done))
