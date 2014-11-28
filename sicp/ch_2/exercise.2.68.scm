#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.68:

;; The `encode' procedure takes as arguments a message and a tree and
;; produces the list of bits that gives the encoded message.

(define (encode message tree)
  (if (null? message) null
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; `Encode-symbol' is a procedure, which you must write, that returns
;; the list of bits that encodes a given symbol according to a given
;; tree.  You should design `encode-symbol' so that it signals an
;; error if the symbol is not in the tree at all.  Test your
;; procedure by encoding the result you obtained in *Note Exercise
;; 2-67:: with the sample tree and seeing whether it is the same as
;; the original sample message.

(require "../lib/huffman.scm")

(define (encode-symbol s tree)
  (define (find-path tree path)
    (cond ((null? tree) path)
          ((equal? (list s) (symbols tree)) path)
          ((member s (symbols (left-branch  tree)))
           (find-path (left-branch  tree) (append path '(0))))
          ((member s (symbols (right-branch tree)))
           (find-path (right-branch tree) (append path '(1))))
          (else (error "symbol ~s is not a member of the encoding" s))))
  (find-path tree '()))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0
                         1 1 0
                         0
                         1 0
                         1 0
                         1 1 1
                         0))

;; I don't know how to assert-error yet, so I won't test those edge cases
(assert-equal null     (encode null sample-tree))
(assert-equal '(0)     (encode '(A) sample-tree))
(assert-equal '(1 0)   (encode '(B) sample-tree))
(assert-equal '(1 1 1) (encode '(C) sample-tree))
(assert-equal '(1 1 0) (encode '(D) sample-tree))
(assert-equal sample-message
              (encode '(A D A B B C A) sample-tree))
(done)
