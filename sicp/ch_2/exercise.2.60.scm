(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.60:

;; We specified that a set would be represented as a list with no
;; duplicates. Now suppose we allow duplicates. For instance, the set
;; {1,2,3} could be represented as the list `(2 3 2 1 3 2 2)'. Design
;; procedures `element-of-set?', `adjoin-set', `union-set', and
;; `intersection-set' that operate on this representation.

(define (union-set s1 s2)
  (if (null? s2) s1
      (union-set (adjoin-set (car s2) s1) (cdr s2))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        (else (intersection-set (cdr set1) set2))))

;; How does the efficiency of each compare with the corresponding
;; procedure for the non-duplicate representation? Are there
;; applications for which you would use this representation in
;; preference to the non-duplicate one?

;; adjoin-set is now O(1), speeding up union-set and :. union-set by a
;; fair margin. intersection-set is however, fucked.
;;
;; No, I can't think of any place where I'd rather sacrifice some
;; speed for clarity. These aren't sets, they're bags.

(assert-equal '(1 2 3)   (sort-< (union-set '(3) '(1 2))))
(assert-equal '(1 1 2 3) (sort-< (union-set '(1 3) '(1 2))))
(done)

;; (assert-equal x y)
(done)
