
(use test)
;; (require 'circuits)
;; (import circuits)

;;; Exercise 3.28

;; Define an or-gate as a primitive function box.
;; Your `or-gate' constructor should be similar to `and-gate'.

(define (get-signal x) x)
(define (add-action! x f) (f))
(define (after-delay delay f) (f))
(define or-gate-delay 0)
(define (set-signal! var val) (set-car! var val))

(define (or-gate a1 a2 output)
  (define (logical-or a b)
    (if (or (= a 1) (= b 1))
        1
        0))
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(test-group "3.28"
            (define (assert-or a b expected)
              (define result '(()))
              (or-gate a b result)
              (test expected (car result)))

            (assert-or 0 0 0)
            (assert-or 1 0 1)
            (assert-or 0 1 1)
            (assert-or 1 1 1))
