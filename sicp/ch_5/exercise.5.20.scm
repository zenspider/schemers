#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 5.20

;; Draw the box-and-pointer representation and the
;; memory-vector representation (as in *Note Figure 5-14::) of the
;; list structure produced by
;;
;;      (define x (cons 1 2))
;;      (define y (list x x))
;;
;; with the `free' pointer initially `p1'.  What is the final value of
;; `free' ?  What pointers represent the values of `x' and `y' ?

;;    +---+---+   +---+---+
;; y: | * | *---->| * | X |
;;    +-|-+---+   +-|-+---+
;;      |           |
;;      |-----------+
;;      |
;;    +-v-+---+
;; x: | * | * |
;;    +-|-+-|-+
;;      |    \
;;      |     \
;;    +-v-+  +-v-+
;;    | 1 |  | 2 |
;;    +---+  +---+
;;
;;            0   1   2   3   4   5   6   7   8   9
;;          +---+---+---+---+---+---+---+---+---+---+
;; the-cars |   | n1| p1| p1|   |   |   |   |   |   |
;;          +---+---+---+---+---+---+---+---+---+---+
;; the-cdrs |   | n2| p3| e0|   |   |   |   |   |   |
;;          +---+---+---+---+---+---+---+---+---+---+
