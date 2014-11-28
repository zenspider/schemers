#!/usr/bin/env csi -s

(require rackunit srfi-1)
(use amb amb-extras)

;;; Exercise 4.40

;; In the multiple dwelling problem, how many sets of assignments are
;; there of people to floors, both before and after the requirement
;; that floor assignments be distinct? It is very inefficient to
;; generate all possible assignments of people to floors and then
;; leave it to backtracking to eliminate them. For example, most of
;; the restrictions depend on only one or two of the person-floor
;; variables, and can thus be imposed before floors have been selected
;; for all the people. Write and demonstrate a much more efficient
;; nondeterministic procedure that solves this problem based upon
;; generating only those possibilities that are not already ruled out
;; by previous restrictions. (Hint: This will require a nest of `let'
;; expressions.)

(define (multiple-dwelling)
  (let ((fletcher                           (amb   2 3 4  )))
    (let ((cooper (cond ((= fletcher 2)     (amb       4 5))
                        ((= fletcher 3)     (amb         5))
                        ((= fletcher 4)     (amb   2      )))))
      (let ((miller (cond ((= cooper 2)     (amb     3 4 5))
                          ((= cooper 4)     (amb         5))
                          ((= cooper 5)     (amb          )))))
        (let ((baker                        (amb 1 2 3 4  ))
              (smith  (cond ((= fletcher 2) (amb       4 5))
                            ((= fletcher 3) (amb 1       5))
                            ((= fletcher 4) (amb 1 2      )))))

          (amb-assert (distinct? (list baker cooper fletcher miller smith)))

          (list (list 'baker    baker)
                (list 'cooper   cooper)
                (list 'fletcher fletcher)
                (list 'miller   miller)
                (list 'smith    smith)))))))

;; original                                  = 1716 checks
;; previous                                  = 1614 checks
;; removal of single floor restrictions      = 1271 checks
;; reduce cooper and smith based on fletcher =  167 checks
;; reordering fletcher miller and cooper     =   52 checks
;; expanding miller based on cooper          =   32 checks

;; This sucks:
;;
;; (cond ((= cooper 2) (amb     3 4 5))
;;       ((= cooper 4) (amb         5))
;;       ((= cooper 5) (amb          )))
;;
;; I'd rather write something like:
;;
;; (define (amb-filter (f . vals))
;;   (amb (splat (filter f vals)))
;;
;; (amb-filter (lambda (n) (> n cooper)) floors)

(define-syntax amb-filter
  (syntax-rules ()
    [(_ f l)
     (eval `(amb . ,(filter f l)))])) ;; wtf? how to do this w/o eval?

(define (awesome-multiple-dwelling)
  (define (!= x y) (not (= x y)))
  (define (non-adjacent x y) (> (abs (- x y)) 1))
  (let* ((l '(1 2 3 4 5))
         (fletcher (amb 2 3 4))
         (cooper   (amb-filter (lambda (n) (non-adjacent n fletcher)) (cdr l)))
         (miller   (amb-filter (lambda (n) (> n cooper))              l))
         (baker    (amb-filter (lambda (n) (!= n 5))                  l))
         (smith    (amb-filter (lambda (n) (non-adjacent n fletcher)) l)))

    (amb-assert (distinct? (list baker cooper fletcher miller smith)))

    (list (list 'baker    baker)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

(test '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
      (all-of (multiple-dwelling)))

;; 32 comparisons! rawr

(test '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))
      (all-of (awesome-multiple-dwelling)))
