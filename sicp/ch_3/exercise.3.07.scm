
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.7

;; Consider the bank account objects created by
;; `make-account', with the password modification described in *Note
;; Exercise 3-3::.  Suppose that our banking system requires the
;; ability to make joint accounts.  Define a procedure `make-joint'
;; that accomplishes this.  `Make-joint' should take three arguments.
;; The first is a password-protected account.  The second argument
;; must match the password with which the account was defined in
;; order for the `make-joint' operation to proceed.  The third
;; argument is a new password.  `Make-joint' is to create an
;; additional access to the original account using the new password.
;; For example, if `peter-acc' is a bank account with password
;; `open-sesame', then
;;
;;      (define paul-acc
;;        (make-joint peter-acc 'open-sesame 'rosebud))
;;
;; will allow one to make transactions on `peter-acc' using the name
;; `paul-acc' and the password `rosebud'.  You may wish to modify your
;; solution to *Note Exercise 3-3:: to accommodate this new feature

(define (make-account balance passwords)
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

  ;; This is a stupid simple way to do it. I don't think the banks
  ;; would ever model it this way. Another way would be to make a
  ;; make-ledger function that returns a shared lambda between them.
  ;; Then an account is just a password system and delegates
  ;; everything to the ledger.

  (set! passwords (list passwords))     ; hah!
  (define (add-password password)
    (set! passwords (cons password passwords)))

  (define (dispatch p m)
    (if (member p passwords)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'add-password) add-password)
              (else (error "Unknown message: " m)))
        bad))

  dispatch)

(define (make-joint account password new-password)
  ((account password 'add-password) new-password)
  account)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc  (make-joint peter-acc 'open-sesame 'rosebud))

(assert-equal 90 ((peter-acc 'open-sesame 'withdraw) 10))
(assert-equal 80 ((paul-acc  'rosebud     'withdraw) 10))
(assert-equal 70 ((peter-acc 'open-sesame 'withdraw) 10))
(done)
