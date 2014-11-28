#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; (require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Exercise 2.50

;; Define the transformation `flip-horiz', which flips painters
;; horizontally, and transformations that rotate painters
;; counterclockwise by 180 degrees and 270 degrees.

;; (define vec make-vect)
;;
;; (define (transform-painter2 painter origin corner1 corner2)
;;   (lambda (frame)
;;     (let ((m (frame-coord-map frame)))
;;       (let ((new-origin (m origin)))
;;         (painter (make-frame new-origin
;;                              (vector-sub (m corner1) new-origin)
;;                              (vector-sub (m corner2) new-origin)))))))
;;
;;
;; (define (flip-vert2 painter)
;;   (transform-painter2 painter
;;                       (vec 0.0 1.0)
;;                       (vec 1.0 1.0)
;;                       (vec 0.0 0.0)))
;;
;; (define (flip-horiz2 painter)
;;   (transform-painter2 painter
;;                       (vec 1.0 0.0)
;;                       (vec 0.0 0.0)
;;                       (vec 1.0 1.0)))
;;
;; (define (rotate90-2 painter)
;;   (transform-painter2 painter
;;                       (vec 1.0 0.0)
;;                       (vec 1.0 1.0)
;;                       (vec 0.0 0.0)))
;;
;; (define (rotate180-2 painter)
;;   (transform-painter2 painter
;;                       (vec 1.0 1.0)
;;                       (vec 0.0 1.0)
;;                       (vec 1.0 0.0)))
;;
;; (define (rotate270-2 painter)
;;   (transform-painter2 painter
;;                       (vec 0.0 1.0)
;;                       (vec 0.0 0.0)
;;                       (vec 1.0 1.0)))
;;
;; ;; (paint einstein)
;; ;; (paint (flip-vert2 einstein))
;; ;; (paint (flip-horiz2 einstein))
;; ;; (paint (rotate90-2 einstein))
;; ;; (paint (rotate180-2 einstein))
;; ;; (paint (rotate270-2 einstein))
