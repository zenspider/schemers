
;;; Exercise 2.10:

;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
;; shoulder and comments that it is not clear what it means to divide
;; by an interval that spans zero. Modify Alyssa's code to check for
;; this condition and to signal an error if it occurs.

(require 'testes)
(import testes)

(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; huh... I read the problem entirely different from aja. I read "an
;; interval that spans zero" as "a zero width interval", not "an
;; interval with a negative lower bound and positive upper bound"
;;
;; don't care...

(define (div-interval x y)
  (if (= (interval-width y) 0) null
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define i     (make-interval 1 2))
(define izero (make-interval 1 1))

(div-interval i izero)                  ; ()
