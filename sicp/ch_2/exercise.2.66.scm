(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.66:

;; Implement the `lookup' procedure for the case where the set of
;; records is structured as a binary tree, ordered by the numerical
;; values of the keys.

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (lookup given-key tree)
  (if (null? tree) #f
      (let ((node (entry tree)))
        (let ((k (key node)))
          (cond ((equal? given-key k) node)
                ((< given-key k)
                 (lookup given-key (left-branch tree)))
                (else (lookup given-key (right-branch tree))))))))

(define key car)

(define db '((2 blicky) ((1 icky) () ()) ((3 woot) () ())))

(assert-equal #f          (lookup 0 db))
(assert-equal '(1 icky)   (lookup 1 db))
(assert-equal '(2 blicky) (lookup 2 db))
(assert-equal '(3 woot)   (lookup 3 db))
(assert-equal #f          (lookup 4 db))
(done)
