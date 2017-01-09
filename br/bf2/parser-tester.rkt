#lang br

(require "parser.rkt")

(module+ test
  (require rackunit)

  (check-equal? (parse-tree "++++-+++-++-++[>++++-+++-++-++<-]>.")
                '(bf-program
                  (op "+") (op "+") (op "+") (op "+")
                  (op "-")
                  (op "+") (op "+") (op "+")
                  (op "-")
                  (op "+") (op "+")
                  (op "-")
                  (op "+") (op "+")
                  (loop "["
                        (op ">")
                        (op "+") (op "+") (op "+") (op "+")
                        (op "-")
                        (op "+") (op "+") (op "+")
                        (op "-")
                        (op "+") (op "+")
                        (op "-")
                        (op "+") (op "+")
                        (op "<")
                        (op "-")
                        "]")
                  (op ">")
                  (op ".")))
  (displayln 'done))
