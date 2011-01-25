
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.75

;; Unfortunately, Alyssa's zero-crossing detector in
;; *Note Exercise 3-74:: proves to be insufficient, because the noisy
;; signal from the sensor leads to spurious zero crossings.  Lem E.
;; Tweakit, a hardware specialist, suggests that Alyssa smooth the
;; signal to filter out the noise before extracting the zero
;; crossings.  Alyssa takes his advice and decides to extract the
;; zero crossings from the signal constructed by averaging each value
;; of the sense data with the previous value.  She explains the
;; problem to her assistant, Louis Reasoner, who attempts to
;; implement the idea, altering Alyssa's program as follows:
;;
;;      (define (make-zero-crossings input-stream last-value)
;;        (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;;          (cons-stream (sign-change-detector avpt last-value)
;;                       (make-zero-crossings (stream-cdr input-stream)
;;                                            avpt))))
;;
;; This does not correctly implement Alyssa's plan.  Find the bug
;; that Louis has installed and fix it without changing the structure
;; of the program.  (Hint: You will need to increase the number of
;; arguments to `make-zero-crossings'.)

;; (assert-equal x y)
(done)
