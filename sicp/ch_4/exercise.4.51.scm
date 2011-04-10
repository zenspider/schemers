#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.51

;; Implement a new kind of assignment called
;; `permanent-set!' that is not undone upon failure.  For example, we
;; can choose two distinct elements from a list and count the number
;; of trials required to make a successful choice as follows:
;;
;;      (define count 0)
;;
;;      (let ((x (an-element-of '(a b c)))
;;            (y (an-element-of '(a b c))))
;;        (permanent-set! count (+ count 1))
;;        (require (not (eq? x y)))
;;        (list x y count))
;;      ;;; Starting a new problem
;;      ;;; Amb-Eval value:
;;      (a b 2)
;;
;;      ;;; Amb-Eval input:
;;      try-again
;;      ;;; Amb-Eval value:
;;      (a c 3)
;;
;; What values would have been displayed if we had used `set!' here
;; rather than `permanent-set!' ?

;; Hrm... something is wrong. Using:

;; (define (require p)
;;   (if (not p) (amb)))
;;
;; (define (an-element-of items)
;;   (printf "an-element-of: ~s~N" items)
;;   (require (not (null? items)))
;;   (amb (car items) (an-element-of (cdr items))))
;;
;; (define count 0)
;;
;; (let ((x (ramb 'a 'b 'c))
;;       (y (ramb 'a 'b 'c)))
;;   (printf "wtf: ~s ~s ~s~N" x y count)
;;   ;; (permanent-set count (+ count 1))
;;   (set count (+ count 1))
;;   (require (not (eq? x y)))
;;   (list x y count))
;;
;; try-again

;;;;;
;; with set I get:
;; ;;; Starting a new problem wtf: c a 0
;; wtf: c b 1
;; wtf: c c 2
;; wtf: b a 3
;; wtf: b b 4
;; wtf: b c 5
;; wtf: a a 6
;; wtf: a b 7
;; wtf: a c 8
;; ;;; There are no more values of
;; (let ((x (ramb (quote a) (quote b) (quote c))) (y (ramb (quote a) (quote b) (quote c)))) (printf wtf: ~s ~s ~s~N x y count) (set count (+ count 1)) (require (not (eq? x y))) (list x y count))

;;;;;
;; and with permanent-set I get:

;; ;;; Starting a new problem wtf: a a 0
;; wtf: a b 0
;;
;; ;;; Amb-Eval value:
;; (a b 0)
;;
;; ;;; Amb-Eval input:
;; wtf: a c 0
;;
;; ;;; Amb-Eval value:
;; (a c 0)

;;;;;
;; I used ramb instead of an-element-of because an-element-of was
;; clearly running through all possibilities immediately. I assumed
;; that was because an-element-of's amb was not being treated like a
;; special form causing the not-null to always bottom out. Apparently
;; that's not the case because after switching to ramb I'm seeing the
;; same thing. No clue. I give up for now