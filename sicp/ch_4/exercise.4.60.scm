#!/usr/bin/env csi -s

(require rackunit)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.60

;; By giving the query
;;
;;      (lives-near ?person (Hacker Alyssa P))
;;
;; Alyssa P. Hacker is able to find people who live near her, with
;; whom she can ride to work.  On the other hand, when she tries to
;; find all pairs of people who live near each other by querying
;;
;;      (lives-near ?person-1 ?person-2)
;;
;; she notices that each pair of people who live near each other is
;; listed twice; for example,
;;
;;      (lives-near (Hacker Alyssa P) (Fect Cy D))
;;      (lives-near (Fect Cy D) (Hacker Alyssa P))
;;
;; Why does this happen?  Is there a way to find a list of people who
;; live near each other, in which each pair appears only once?
;; Explain.

;; A: Simple, the lives-near query is commutative. if A lives near B,
;;    then B lives near A. Since the query engine tests every
;;    combination, it makes sense you'd get both.

(define (list<? l1 l2)
  (or (null? l1)
      (string<? (symbol->string (car l1))
                (symbol->string (car l2)))
      (and (string=? (symbol->string (car l1))
                     (symbol->string (car l2)))
           (list<? (cdr l1) (cdr l2)))))

(assert! '(rule (lives-near2 ?person-1 ?person-2)
                (and (address ?person-1 (?town . ?rest-1))
                     (address ?person-2 (?town . ?rest-2))
                     (not (same ?person-1 ?person-2))
                     (lisp-value list<? ?person-1 ?person-2))))

(test '((lives-near (Aull DeWitt)     (Reasoner Louis))
        (lives-near (Aull DeWitt)     (Bitdiddle Ben))
        (lives-near (Reasoner Louis)  (Aull DeWitt))
        (lives-near (Reasoner Louis)  (Bitdiddle Ben))
        (lives-near (Hacker Alyssa P) (Fect Cy D))
        (lives-near (Fect Cy D)       (Hacker Alyssa P))
        (lives-near (Bitdiddle Ben)   (Aull DeWitt))
        (lives-near (Bitdiddle Ben)   (Reasoner Louis)))
      (all-of '(lives-near ?a ?b)))

(test '((lives-near2 (Aull DeWitt)   (Reasoner Louis))
        (lives-near2 (Aull DeWitt)   (Bitdiddle Ben))
        (lives-near2 (Fect Cy D)     (Hacker Alyssa P))
        (lives-near2 (Bitdiddle Ben) (Reasoner Louis)))
      (all-of '(lives-near2 ?a ?b)))
