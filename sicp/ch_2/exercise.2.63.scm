(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.63:

;; Each of the following two procedures converts a binary tree to a
;; list.

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (tree->list-1 tree)
  (if (null? tree) '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree) result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;;   a. Do the two procedures produce the same result for every tree?
;;      If not, how do the results differ?

;; A: yes. tree->list-1 is just the recursive incarnation of tree->list-2

;;      What lists do the two procedures produce for the trees in
;;      *Note Figure 2-16::?


;;    7          3             5
;;    /\         /\            /\
;;   3  9       1  7          3  9
;;  /\   \         /\        /   /\
;; 1  5  11       5  9      1   7  11
;;                    \
;;                    11

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(define expected '(1 3 5 7 9 11))
(assert-many (lambda (f)
               (assert-equal expected (f t1))
               (assert-equal expected (f t2))
               (assert-equal expected (f t3)))
             tree->list-1
             tree->list-2)

;;   b. Do the two procedures have the same order of growth in the
;;      number of steps required to convert a balanced tree with n
;;      elements to a list?  If not, which one grows more slowly?

;; This may be a trick question... normally the recursive version will
;; have a larger number of steps, but for a balanced tree? I think
;; it'll still come out in favor of the iterative version, but I'd
;; much rather write a benchmark to prove that... I'm not gonna, but
;; I'd rather. :)

(done)
