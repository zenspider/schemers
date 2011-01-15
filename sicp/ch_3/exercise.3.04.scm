#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.4

;; Modify the `make-account' procedure of *Note
;; Exercise 3-3:: by adding another local state variable so that, if
;; an account is accessed more than seven consecutive times with an
;; incorrect password, it invokes the procedure `call-the-cops'.

(define (make-account balance password)
  (let ((errors 0))
    (define (call-the-cops) "HALP! POLICE!!!")

    (define (bad _)
      (set! errors (+ errors 1))
      (if (> errors 7)
          (call-the-cops)
          "Incorrect password"))

    (define (good) (set! errors 0))

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
          (begin
            (good)
            (cond ((eq? 'withdraw m) withdraw)
                  ((eq? 'deposit  m) deposit)
                  (else (error "Unknown message: " m))))
          bad))
    dispatch))

(define acc (make-account 100 'secret-password))

(assert-equal 60 ((acc 'secret-password 'withdraw) 40))
(assert-equal 70 ((acc 'secret-password 'deposit) 10))

(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "Incorrect password" ((acc 'some-other-password 'deposit) 50))
(assert-equal "HALP! POLICE!!!"    ((acc 'some-other-password 'deposit) 50))
(done)
