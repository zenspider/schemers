#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.41

;; Ben Bitdiddle worries that it would be better to
;; implement the bank account as follows (where the commented line
;; has been changed):
;;
;;      (define (make-account balance)
;;        (define (withdraw amount)
;;          (if (>= balance amount)
;;              (begin (set! balance (- balance amount))
;;                     balance)
;;              "Insufficient funds"))
;;        (define (deposit amount)
;;          (set! balance (+ balance amount))
;;          balance)
;;        ;; continued on next page
;;
;;        (let ((protected (make-serializer)))
;;          (define (dispatch m)
;;            (cond ((eq? m 'withdraw) (protected withdraw))
;;                  ((eq? m 'deposit) (protected deposit))
;;                  ((eq? m 'balance)
;;                   ((protected (lambda () balance)))) ; serialized
;;                  (else (error "Unknown request -- MAKE-ACCOUNT"
;;                               m))))
;;          dispatch))
;;
;; because allowing unserialized access to the bank balance can
;; result in anomalous behavior.  Do you agree?  Is there any
;; scenario that demonstrates Ben's concern?

;; (assert-equal x y)
(done)
