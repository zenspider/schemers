
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.35

;; Ben Bitdiddle tells Louis that one way to avoid
;; the trouble in *Note Exercise 3-34:: is to define a squarer as a
;; new primitive constraint.  Fill in the missing portions in Ben's
;; outline for a procedure to implement such a constraint:
;;
;;      (define (squarer a b)
;;        (define (process-new-value)
;;          (if (has-value? b)
;;              (if (< (get-value b) 0)
;;                  (error "square less than 0 -- SQUARER" (get-value b))
;;                  <ALTERNATIVE1>)
;;              <ALTERNATIVE2>))
;;        (define (process-forget-value) <BODY1>)
;;        (define (me request) <BODY2>)
;;        <REST OF DEFINITION>
;;        me)

;; (assert-equal x y)
(done)
