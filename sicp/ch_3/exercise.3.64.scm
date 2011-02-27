(use test)
(require-library streams)
(import streams)
(use numbers)

;;; Exercise 3.64

;; Write a procedure `stream-limit' that takes as
;; arguments a stream and a number (the tolerance).  It should
;; examine the stream until it finds two successive elements that
;; differ in absolute value by less than the tolerance, and return
;; the second of the two elements.  Using this, we could compute
;; square roots up to a given tolerance by
;;
;;      (define (sqrt x tolerance)
;;        (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s delta)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s1 s0)) delta) s1
        (stream-limit (stream-cdr s) delta))))

(define nths (stream-map (lambda (n) (/ 1 n)) integers))

(test 1/33 (stream-limit nths 1/1000))
