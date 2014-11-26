;;; Exercise 2.14:

;; After considerable work, Alyssa P. Hacker delivers her finished
;; system. Several years later, after she has forgotten all about it,
;; she gets a frenzied call from an irate user, Lem E. Tweakit. It
;; seems that Lem has noticed that the formula for parallel resistors
;; can be written in two algebraically equivalent ways:
;;
;;       R_1 R_2
;;      ---------
;;      R_1 + R_2
;;
;; and
;;
;;            1
;;      -------------
;;      1/R_1 + 1/R_2
;;
;; He has written the following two programs, each of which computes
;; the parallel-resistors formula differently:

(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; Lem complains that Alyssa's program gives different answers for the
;; two ways of computing. This is a serious complaint.
;;
;; Demonstrate that Lem is right. Investigate the behavior of the
;; system on a variety of arithmetic expressions. Make some intervals
;; A and B, and use them in computing the expressions A/A and A/B. You
;; will get the most insight by using intervals whose width is a small
;; percentage of the center value. Examine the results of the
;; computation in center-percent form (see *Note Exercise 2-12::).

(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define a (make-center-percent 1 1/100000))
(define b (make-center-percent 2 1/100000))

a                  ; '(99999/100000 . 100001/100000)
b                  ; '(99999/50000  . 100001/50000)

(par1 a b)         ; '(3333266667/5000050000 . 10000200001/14999850000)
(par2 a b)         ; '(33333/50000 . 100001/150000)

;; zomg!

(div-interval a a) ; '(99999/100001 . 100001/99999)
(div-interval a b) ; '(99999/200002 . 100001/199998)
