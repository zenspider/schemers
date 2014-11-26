(require "../lib/test.rkt")

;;; Exercise 2.24.

;; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))).
;; Give the result printed by the interpreter, the corresponding
;; box-and-pointers structure, and the interpretation of this as a tree
;; (as in figure 2.6).

(assert-equal '(1 (2 (3 4))) (list 1 (list 2 (list 3 4))))
(done)

;;
;;                +---+---+   +---+---+
;;  (1 (2 (3 4))) | * | *-+-->| * | / |
;;                +-|-+---+   +-+-+---+
;;                  |           |
;;                +-v-+       +---+---+   +---+---+
;;                | 1 |       | * | *-+-->| * | / |
;;                +---+       +-|-+---+   +-+-+---+
;;                              |           |
;;                            +-v-+       +---+---+   +---+---+
;;                  (2 (3 4)) | 2 |       | * | *-+-->| * | / |
;;                            +---+       +-|-+---+   +-+-+---+
;;                                          |           |
;;                                        +-v-+       +-v-+
;;                                  (3 4) | 3 |       | 4 |
;;                                        +---+       +-+-+
;;
;;
;;                  (1 (2 (3 4)))
;;                   /\
;;                  1 (2 (3 4))
;;                     /\
;;                    2  (3 4)
;;                        / \
;;                       3   4
;;
