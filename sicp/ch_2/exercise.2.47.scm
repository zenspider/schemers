
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;; Exercise 2.47

;; Here are two possible constructors for frames:
;;
;;      (define (make-frame origin edge1 edge2)
;;        (list origin edge1 edge2))
;;
;;      (define (make-frame origin edge1 edge2)
;;        (cons origin (cons edge1 edge2)))
;;
;; For each constructor supply the appropriate selectors to produce an
;; implementation for frames.

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame1 car)
(define edge1-frame1 cadr)
(define edge2-frame1 caddr)

(define origin-frame2 car)
(define edge1-frame2 cadr)
(define edge2-frame2 cddr)

(let ((f1 (make-frame1 '(0 . 0) '(1 . 0) '(0 . 2)))
      (f2 (make-frame2 '(0 . 0) '(1 . 0) '(0 . 2))))
  (assert-equal '(0 . 0) (origin-frame1 f1))
  (assert-equal '(1 . 0) (edge1-frame1 f1))
  (assert-equal '(0 . 2) (edge2-frame1 f1))

  (assert-equal '(0 . 0) (origin-frame2 f2))
  (assert-equal '(1 . 0) (edge1-frame2 f2))
  (assert-equal '(0 . 2) (edge2-frame2 f2)))

(done)
