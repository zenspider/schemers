#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")
(require "../lib/huffman.rkt")

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
        ((< (weight x) (weight (car set))) (cons x set))
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

  ;; make-leaf-set
  (assert-equal (list d c b a)
                (make-leaf-set '((A 8) (B 3) (C 1) (D 1))))

  ;; successive-merge
  (let ((cd (make-code-tree c d))
        (bc (make-code-tree b c))
        (ef (make-code-tree e f))
        (de (make-code-tree d e))
        (fg (make-code-tree f g))
        (gh (make-code-tree g h)))

    (let ((cd_ef (make-code-tree cd ef))
          (gh_cd_ef (make-code-tree
                     gh
                     (make-code-tree cd ef)))
          (hg_fe_dcb (make-code-tree
                      (make-code-tree
                       (make-code-tree h g)
                       (make-code-tree f e))
                      (make-code-tree
                       (make-code-tree d c)
                       b))))

      (assert-equal cd        (successive-merge (list c d)))
      (assert-equal cd_ef     (successive-merge (list c d e f)))
      (assert-equal gh_cd_ef  (successive-merge (list c d e f g h)))
      (assert-equal hg_fe_dcb (successive-merge (list h g f e d c b)))

      ;; Initial leaves {(A 8)  (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)}
      ;; Merge          {(A 8)  (B 3) ({C D} 2)   (E 1) (F 1) (G 1) (H 1)}
      ;; Merge          {(A 8)  (B 3) ({C D} 2)  ({E F} 2)    (G 1) (H 1)}
      ;; Merge          {(A 8)  (B 3) ({C D} 2)  ({E F} 2)   ({G H} 2)}
      ;; Merge          {(A 8)  (B 3) ({C D} 2)  ({E F G H} 4)}
      ;; Merge          {(A 8) ({B C D} 5)       ({E F G H} 4)}
      ;; Merge          {(A 8) ({B C D E F G H} 9)}
      ;; Final merge    {({A B C D E F G H} 17)}

      ;; LIAR!

      (define expected
        (make-code-tree
         a
         (make-code-tree
          (make-code-tree
           (make-code-tree h g)
           (make-code-tree f e))
          (make-code-tree
           (make-code-tree d c)
           b))))

      (assert-equal expected (successive-merge (list h g f e d c b a))))))
(done)
