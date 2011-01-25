
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.3

;; Modify the `make-account' procedure so that it
;; creates password-protected accounts.  That is, `make-account'
;; should take a symbol as an additional argument, as in
;;
;;      (define acc (make-account 100 'secret-password))
;;
;; The resulting account object should process a request only if it
;; is accompanied by the password with which the account was created,
;; and should otherwise return a complaint:
;;
;;      ((acc 'secret-password 'withdraw) 40)
;;      60
;;
;;      ((acc 'some-other-password 'deposit) 50)
;;      "Incorrect password"

(define (make-account balance password)
  (define (bad _) "Incorrect password")

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Issufficient funds: "))

  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? 'withdraw m) withdraw)
              ((eq? 'deposit  m) deposit)
              (else (error "Unknown message: " m)))
        bad))
  dispatch)

(define acc (make-account 100 'secret-password))

(assert-equal 60 ((acc 'secret-password 'withdraw) 40))
(assert-equal 70 ((acc 'secret-password 'deposit) 10))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(done)
