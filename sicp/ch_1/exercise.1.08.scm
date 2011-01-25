
;;; *Exercise 1.8:*
;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x, then a better approximation is
;; given by the value
;;
;;      x/y^2 + 2y
;;      ----------
;;          3
;;
;; Use this formula to implement a cube-root procedure analogous to
;; the square-root procedure. (In section *Note 1-3-4:: we will see
;; how to implement Newton's method in general as an abstraction of
;; these square-root and cube-root procedures.)

(define (improve x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (good-enough? x y old-y)
  (< (abs (- old-y y)) (/ y 1000000)))

(define (cube-root-iter x y old-y)
  (if (good-enough? x y old-y)
      y
      (cube-root-iter x (improve x y) y)))

(define (cube-root x)
  (cube-root-iter x 1.0 0.0))

(cube-root 27)                          ;  3.0000000000000977
(cube-root 1000)                        ; 10.000000000000002
