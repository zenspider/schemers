#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.50

;; Implement a new special form `ramb' that is like `amb' except that
;; it searches alternatives in a random order, rather than from left
;; to right. Show how this can help with Alyssa's problem in *Note
;; Exercise 4-49::.

;; (ramb implemented in amb-eval.scm)

;; I didn't do 4.49, so I can't show that. I can demonstrate that
;; ramb's output is random and that dwelling still passes using:

;; (define (not p)
;;   (if p false true))
;;
;; (define (amb-assert p)
;;   (if (not p) (amb) true))
;;
;; (define (distinct? l)
;;   (define (iterate l seen)
;;     (cond ((null? l) true)
;;           ((member (car l) seen) false)
;;           (else (iterate (cdr l) (cons (car l) seen)))))
;;   (iterate l '()))
;;
;; (define (multiple-dwelling)
;;   (let ((baker    (ramb 1 2 3 4 5))
;;         (cooper   (ramb 1 2 3 4 5))
;;         (fletcher (ramb 1 2 3 4 5))
;;         (miller   (ramb 1 2 3 4 5))
;;         (smith    (ramb 1 2 3 4 5)))
;;
;;     (amb-assert (distinct? (list baker cooper fletcher miller smith))) ; 1
;;     (amb-assert (not (= (abs (- smith fletcher)) 1)))                  ; 7
;;     (amb-assert (not (= (abs (- fletcher cooper)) 1)))                 ; 8
;;     (amb-assert (> miller cooper))                                     ; 6
;;     (amb-assert (not (= baker 5)))                                     ; 2
;;     (amb-assert (not (= cooper 1)))                                    ; 3
;;     (amb-assert (not (= fletcher 5)))                                  ; 4
;;     (amb-assert (not (= fletcher 1)))                                  ; 4
;;
;;     (list (list 'baker    baker)
;;           (list 'cooper   cooper)
;;           (list 'fletcher fletcher)
;;           (list 'miller   miller)
;;           (list 'smith    smith))))
;;
;; (multiple-dwelling)
;; => '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))

