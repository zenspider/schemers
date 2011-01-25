
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 2.75

;; Implement the constructor `make-from-mag-ang' in
;; message-passing style.  This procedure should be analogous to the
;; `make-from-real-imag' procedure given above.

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(assert-equal 3              ((make-from-real-imag 3 3) 'real-part))
(assert-equal 3              ((make-from-real-imag 3 3) 'imag-part))
(assert-float (* (sqrt 2) 3) ((make-from-real-imag 3 3) 'magnitude))
(assert-float (atan 3 3)     ((make-from-real-imag 3 3) 'angle))

(assert-equal (* 3 (cos 45)) ((make-from-mag-ang 3 45) 'real-part))
(assert-equal (* 3 (sin 45)) ((make-from-mag-ang 3 45) 'imag-part))
(assert-equal 3              ((make-from-mag-ang 3 45) 'magnitude))
(assert-equal 45             ((make-from-mag-ang 3 45) 'angle))

(done)
