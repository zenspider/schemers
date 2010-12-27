#lang racket

(require "../lib/testes.rkt")

;; Exercise 2.29.

;; A binary mobile consists of two branches, a left branch and a right
;; branch. Each branch is a rod of a certain length, from which hangs
;; either a weight or another binary mobile. We can represent a binary
;; mobile using compound data by constructing it from two branches
;; (for example, using list):

;; m = ([l_len w_or_m] [r_len w_or_m])

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number
;; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors left-branch and right-branch,
;; which return the branches of a mobile, and branch-length and
;; branch-structure, which return the components of a branch.

(define branch-length    car)
(define branch-structure cadr)
(define left-branch      car)
(define right-branch     cadr)

(define a (make-branch 1 2))
(define b (make-branch 2 3))
(define m (make-mobile a b))

;; (assert-equal 2 (branch-length b))
;; (assert-equal 3 (branch-structure b))
;; (assert-equal a (left-branch m))
;; (assert-equal b (right-branch m))

;; b. Using your selectors, define a procedure total-weight that
;; returns the total weight of a mobile.

(define (xtotal-weight m)
  (if (null? m) 0
      (+ (cond ((null? (left-branch m)) 0)
               ((and (list? (left-branch m))
                     (list? (branch-structure (left-branch m))))
                (branch-structure (left-branch m)))
               ((list? (left-branch m))
                (branch-structure (left-branch m)))
               (else (branch-structure m)))
         (cond ((null? (right-branch m)) 0)
               ((and (list? (right-branch m))
                     (list? (branch-structure (right-branch m))))
                (branch-structure (right-branch m)))
               ((list? (right-branch m))
                (branch-structure (right-branch m)))
               (else (branch-structure m))))))

(define (total-weight m)
  (if (null? m) 0
      (+ (cond ((null? (left-branch m)) 0)
               ((list? (left-branch m)) (branch-structure (left-branch m)))
               (else (branch-structure m)))
         (cond ((null? (right-branch m)) 0)
               ((list? (right-branch m)) (branch-structure (right-branch m)))
               (else (branch-structure m))))))

(assert-equal 0 (total-weight '()))
(assert-equal 2 (total-weight '((1 2) ())))
(assert-equal 2 (total-weight '(() (1 2))))
(assert-equal 5 (total-weight m))
(assert-equal 6 (total-weight '((1 2) (3 4))))

;; c. A mobile is said to be balanced if the torque applied by its
;; top-left branch is equal to that applied by its top-right branch
;; (that is, if the length of the left rod multiplied by the weight
;; hanging from that rod is equal to the corresponding product for the
;; right side) and if each of the submobiles hanging off its branches
;; is balanced. Design a predicate that tests whether a binary mobile
;; is balanced.

(define (balanced? m)
  (and (= (total-weight (left-branch m))
          (total-weight (right-branch m)))
       (balanced? (left-branch m))
       (balanced? (right-branch m))))

(assert-equal #f (balanced? m))
(assert-equal #f (balanced? '((1 2) ())))
(assert-equal #f (balanced? '(()    (1 2))))
;; (assert-equal #f (balanced? '((1 0) ())))
; (assert-equal #f (balanced? '(()    (1 0))))
; (assert-equal #f (balanced? '((1 2) (3 2))))

;; d. Suppose we change the representation of mobiles so that the
;; constructors are

;; (define (make-mobile left right)
;;   (cons left right))
;;
;; (define (make-branch length structure)
;;   (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?

;; A: The cadrs need to become cdrs...

(done)
