#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.77

;; The `integral' procedure used above was analogous
;; to the "implicit" definition of the infinite stream of integers in
;; section *Note 3-5-2::.  Alternatively, we can give a definition of
;; `integral' that is more like `integers-starting-from' (also in
;; section *Note 3-5-2::):
;;
;;      (define (integral integrand initial-value dt)
;;        (cons-stream initial-value
;;                     (if (stream-null? integrand)
;;                         the-empty-stream
;;                         (integral (stream-cdr integrand)
;;                                   (+ (* dt (stream-car integrand))
;;                                      initial-value)
;;                                   dt))))
;;
;; When used in systems with loops, this procedure has the same
;; problem as does our original version of `integral'.  Modify the
;; procedure so that it expects the `integrand' as a delayed argument
;; and hence can be used in the `solve' procedure shown above.
;;
;; *Figure 3.35:* Signal-flow diagram for the solution to a
;; second-order linear differential equation.
;;
;;                     dy_0                y_0
;;                      |                   |
;;                      V                   V
;;         ddy     +----------+    dy  +----------+    y
;;      +--------->| integral +-----*--+ integral +--*--->
;;      |          +----------+     |  +----------+  |
;;      |                           |                |
;;      |            +----------+   |                |
;;      |     __/|<--+ scale: a |<--+                |
;;      |   _/   |   +----------+                    |
;;      +--<_add |                                   |
;;           \__ |   +----------+                    |
;;              \|<--+ scale: b |<-------------------+
;;                   +----------+

;; (assert-equal x y)
(done)
