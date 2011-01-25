
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)
;; (require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;; Exercise 2.49

;; Use `segments->painter' to define the following primitive painters:
;;
;;   a. The painter that draws the outline of the designated frame.
;;
;;   b. The painter that draws an "X" by connecting opposite corners
;;      of the frame.
;;
;;   c. The painter that draws a diamond shape by connecting the
;;      midpoints of the sides of the frame.
;;
;;   d. The `wave' painter.

;; (define vec make-vect)
;; (define seg make-segment)
;;
;; (define (paint-box)
;;   (let ((s (/   0 128))
;;         (e (/ 127 128)))
;;     (paint (segments->painter (list (seg (vec s s) (vec s e))
;;                                     (seg (vec s e) (vec e e))
;;                                     (seg (vec e e) (vec e s))
;;                                     (seg (vec e s) (vec s s)))))))
;;
;; (define (paint-x)
;;   (let ((s (/   0 128))
;;         (e (/ 127 128)))
;;     (paint (segments->painter (list (seg (vec s s) (vec e e))
;;                                     (seg (vec s e) (vec e s)))))))
;;
;; (define (paint-diamond)
;;   (let ((s (/   0 128))
;;         (e (/ 127 128))
;;         (m (/  64 128)))
;;     (paint (segments->painter (list (seg (vec s m) (vec m e))
;;                                     (seg (vec m e) (vec e m))
;;                                     (seg (vec e m) (vec m s))
;;                                     (seg (vec m s) (vec s m)))))))
;;
;; ;; (paint-box)
;; ;; (paint-x)
;; ;; (paint-diamond)
;;
;; ;; aaaaand... fuck the wave guy
