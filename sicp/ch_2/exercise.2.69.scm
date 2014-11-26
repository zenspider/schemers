(require "../lib/test.rkt")
(require "../lib/myutils.scm")
(require "../lib/huffman.scm")

;;; Exercise 2.69:

;; The following procedure takes as its argument a list of
;; symbol-frequency pairs (where no symbol appears in more than one
;; pair) and generates a Huffman encoding tree according to the
;; Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((<= (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; `Make-leaf-set' is the procedure given above that transforms the
;; list of pairs into an ordered set of leaves.  `Successive-merge'
;; is the procedure you must write, using `make-code-tree' to
;; successively merge the smallest-weight elements of the set until
;; there is only one element left, which is the desired Huffman tree.
;; (This procedure is slightly tricky, but not really complicated.
;; If you find yourself designing a complex procedure, then you are
;; almost certainly doing something wrong.  You can take significant
;; advantage of the fact that we are using an ordered set
;; representation.)

(define (successive-merge pairs)
  (cond ((null? pairs) null)
        ((null? (cdr pairs)) (car pairs))
        (else (let ((a  (car  pairs))
                    (b  (cadr pairs))
                    (c* (cddr pairs)))
                (successive-merge (adjoin-set (make-code-tree a b) c*))))))

(let ((a (make-leaf 'A 8))
      (b (make-leaf 'B 3))
      (c (make-leaf 'C 1))
      (d (make-leaf 'D 1))
      (e (make-leaf 'E 1))
      (f (make-leaf 'F 1))
      (g (make-leaf 'G 1))
      (h (make-leaf 'H 1)))

  (let ((cd (make-code-tree c d))
        (ef (make-code-tree e f))
        (gh (make-code-tree g h))
        (input '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))

    (assert-equal '((leaf C 1) (leaf D 1) (leaf E 1) (leaf F 1)
                    (leaf G 1) (leaf H 1) (leaf B 3) (leaf A 8))
                  (make-leaf-set input))

    ;; successive-merge
    (assert-equal cd
                  (successive-merge (list c d)))

    (assert-equal (make-code-tree cd ef)
                  (successive-merge (list e f c d)))

    (assert-equal (make-code-tree cd (make-code-tree gh ef))
                  (successive-merge (list c d e f g h)))

    (assert-equal (make-code-tree (make-code-tree gh ef)
                                  (make-code-tree cd b))
                  (successive-merge (list c d e f g h b)))

    (assert-equal (make-code-tree a (make-code-tree
                                     (make-code-tree gh ef)
                                     (make-code-tree cd b)))
                  (successive-merge (list c d e f g h b a)))

    (assert-equal (make-code-tree a (make-code-tree
                                     (make-code-tree gh ef)
                                     (make-code-tree cd b)))
                  (generate-huffman-tree input))))
(done)
