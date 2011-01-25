
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 2.71:

;; Suppose we have a Huffman tree for an alphabet of n symbols, and
;; that the relative frequencies of the symbols are 1, 2, 4, ...,
;; 2^(n-1). Sketch the tree for n=5; for n=10. In such a tree (for
;; general n) how many bits are required to encode the most frequent
;; symbol? the least frequent symbol?

;; sketch? fuck you! we just wrote all this damn code...

;; n = 5
;; (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16)))

'(((((leaf a 1)
    (leaf b 2) (a b) 3)
   (leaf c 4) (a b c) 7)
  (leaf d 8) (a b c d) 15)
 (leaf e 16) (a b c d e) 31)

;; (map (lambda (n) (list 'x (expt 2 (- n 1)))) (enumerate-interval 1 10))

;; (generate-huffman-tree
;;  '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))

'((((((((((leaf a 1) (leaf b 2) (a b) 3)
         (leaf c 4) (a b c) 7)
        (leaf d 8) (a b c d) 15)
       (leaf e 16) (a b c d e) 31)
      (leaf f 32) (a b c d e f) 63)
     (leaf g 64) (a b c d e f g) 127)
    (leaf h 128) (a b c d e f g h) 255)
   (leaf i 256) (a b c d e f g h i) 511)
  (leaf j 512) (a b c d e f g h i j) 1023)

;; most frequent is always one: 0
;; least frequent looks like n - 1

;; (assert-equal x y)
(done)
