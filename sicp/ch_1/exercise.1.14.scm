#lang racket/base

;;; Exercise 1.14:

;; Draw the tree illustrating the process generated by the
;; `count-change' procedure of section *Note 1-2-2:: in making change
;; for 11 cents. What are the orders of growth of the space and number
;; of steps used by this process as the amount to be changed
;; increases?

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 10 1
;; 5 5 1
;; 5 1 1 1 1 1 1
;; 1 1 1 1 1 1 1 1 1 1 1

;; (count-change 11)
;; (cc 11 5)
;; (+ (cc 11 4)
;;    (cc (- 11 (first-denomination 5)) 5))
;; (+ (+ (cc 11 3)
;;       (cc (- 11 (first-denomination 4)) 4))
;;    (cc (- 11 (first-denomination 5)) 5))
;; (+ (+ (+ (cc 11 2)
;;          (cc (- 11 (first-denomination 3)) 3))
;;       (cc (- 11 (first-denomination 4)) 4))
;;    (cc (- 11 (first-denomination 5)) 5))
;; (+ (+ (+ (+ (cc 11 1)
;;             (cc (- 11 (first-denomination 2)) 2))
;;          (cc (- 11 (first-denomination 3)) 3))
;;       (cc (- 11 (first-denomination 4)) 4))
;;    (cc (- 11 (first-denomination 5)) 5))
;; (+ (+ (+ (+ (+ (cc 11 0)
;;                (cc (- 11 (first-denomination 1)) 1))
;;             (cc (- 11 (first-denomination 2)) 2))
;;          (cc (- 11 (first-denomination 3)) 3))
;;       (cc (- 11 (first-denomination 4)) 4))
;;    (cc (- 11 (first-denomination 5)) 5))
;; (+ (+ (+ (+ (+ (cc 11 0)
;;                (cc (- 11 (first-denomination 1)) 1))
;;             (cc (- 11 (first-denomination 2)) 2))
;;          (cc (- 11 (first-denomination 3)) 3))
;;       (cc (- 11 (first-denomination 4)) 4))
;;    (cc (- 11 (first-denomination 5)) 5))
;;
;; ;; I don't think I care anymore. I know how recursion works. Bite me.

(count-change 11)

;; oh look, cheating!

;; >(count-change 11)
;; >(cc 11 5)
;; > (cc 11 4)
;; > >(cc 11 3)
;; > > (cc 11 2)
;; > > >(cc 11 1)
;; > > > (cc 11 0)
;; < < < 0
;; > > > (first-denomination 1)
;; < < < 1
;; > > > (cc 10 1)
;; > > > >(cc 10 0)
;; < < < <0
;; > > > >(first-denomination 1)
;; < < < <1
;; > > > >(cc 9 1)
;; > > > > (cc 9 0)
;; < < < < 0
;; > > > > (first-denomination 1)
;; < < < < 1
;; > > > > (cc 8 1)
;; > > > > >(cc 8 0)
;; < < < < <0
;; > > > > >(first-denomination 1)
;; < < < < <1
;; > > > > >(cc 7 1)
;; > > > > > (cc 7 0)
;; < < < < < 0
;; > > > > > (first-denomination 1)
;; < < < < < 1
;; > > > > > (cc 6 1)
;; > > > >[10] (cc 6 0)
;; < < < <[10] 0
;; > > > >[10] (first-denomination 1)
;; < < < <[10] 1
;; > > > >[10] (cc 5 1)
;; > > > >[11] (cc 5 0)
;; < < < <[11] 0
;; > > > >[11] (first-denomination 1)
;; < < < <[11] 1
;; > > > >[11] (cc 4 1)
;; > > > >[12] (cc 4 0)
;; < < < <[12] 0
;; > > > >[12] (first-denomination 1)
;; < < < <[12] 1
;; > > > >[12] (cc 3 1)
;; > > > >[13] (cc 3 0)
;; < < < <[13] 0
;; > > > >[13] (first-denomination 1)
;; < < < <[13] 1
;; > > > >[13] (cc 2 1)
;; > > > >[14] (cc 2 0)
;; < < < <[14] 0
;; > > > >[14] (first-denomination 1)
;; < < < <[14] 1
;; > > > >[14] (cc 1 1)
;; > > > >[15] (cc 1 0)
;; < < < <[15] 0
;; > > > >[15] (first-denomination 1)
;; < < < <[15] 1
;; > > > >[15] (cc 0 1)
;; < < < <[15] 1
;; < < < <[14] 1
;; < < < <[13] 1
;; < < < <[12] 1
;; < < < <[11] 1
;; < < < <[10] 1
;; < < < < < 1
;; < < < < <1
;; < < < < 1
;; < < < <1
;; < < < 1
;; < < <1
;; > > >(first-denomination 2)
;; < < <5
;; > > >(cc 6 2)
;; > > > (cc 6 1)
;; > > > >(cc 6 0)
;; < < < <0
;; > > > >(first-denomination 1)
;; < < < <1
;; > > > >(cc 5 1)
;; > > > > (cc 5 0)
;; < < < < 0
;; > > > > (first-denomination 1)
;; < < < < 1
;; > > > > (cc 4 1)
;; > > > > >(cc 4 0)
;; < < < < <0
;; > > > > >(first-denomination 1)
;; < < < < <1
;; > > > > >(cc 3 1)
;; > > > > > (cc 3 0)
;; < < < < < 0
;; > > > > > (first-denomination 1)
;; < < < < < 1
;; > > > > > (cc 2 1)
;; > > > >[10] (cc 2 0)
;; < < < <[10] 0
;; > > > >[10] (first-denomination 1)
;; < < < <[10] 1
;; > > > >[10] (cc 1 1)
;; > > > >[11] (cc 1 0)
;; < < < <[11] 0
;; > > > >[11] (first-denomination 1)
;; < < < <[11] 1
;; > > > >[11] (cc 0 1)
;; < < < <[11] 1
;; < < < <[10] 1
;; < < < < < 1
;; < < < < <1
;; < < < < 1
;; < < < <1
;; < < < 1
;; > > > (first-denomination 2)
;; < < < 5
;; > > > (cc 1 2)
;; > > > >(cc 1 1)
;; > > > > (cc 1 0)
;; < < < < 0
;; > > > > (first-denomination 1)
;; < < < < 1
;; > > > > (cc 0 1)
;; < < < < 1
;; < < < <1
;; > > > >(first-denomination 2)
;; < < < <5
;; > > > >(cc -4 2)
;; < < < <0
;; < < < 1
;; < < <2
;; < < 3
;; > > (first-denomination 3)
;; < < 10
;; > > (cc 1 3)
;; > > >(cc 1 2)
;; > > > (cc 1 1)
;; > > > >(cc 1 0)
;; < < < <0
;; > > > >(first-denomination 1)
;; < < < <1
;; > > > >(cc 0 1)
;; < < < <1
;; < < < 1
;; > > > (first-denomination 2)
;; < < < 5
;; > > > (cc -4 2)
;; < < < 0
;; < < <1
;; > > >(first-denomination 3)
;; < < <10
;; > > >(cc -9 3)
;; < < <0
;; < < 1
;; < <4
;; > >(first-denomination 4)
;; < <25
;; > >(cc -14 4)
;; < <0
;; < 4
;; > (first-denomination 5)
;; < 50
;; > (cc -39 5)
;; < 0
;; <4
