#lang jsonic
// a line comment
[
  @$ 'null $@,
  @$ (* 6 7) $@,
  @$ (= 2 (+ 1 1)) $@,
  @$ (list "array" "of" "strings") $@,
  @$ (hash 'key-1 'null
           'key-2 (even? 3)
           'key-3 (hash 'subkey 21)) $@
]
