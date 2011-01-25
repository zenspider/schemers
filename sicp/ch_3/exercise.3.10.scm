
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.10

;; In the `make-withdraw' procedure, the local
;; variable `balance' is created as a parameter of `make-withdraw'.
;; We could also create the local state variable explicitly, using
;; `let', as follows:

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; Recall from section *Note 1-3-2:: that `let' is simply syntactic
;; sugar for a procedure call:
;;
;;      (let ((<VAR> <EXP>)) <BODY>)
;;
;; is interpreted as an alternate syntax for
;;
;;      ((lambda (<VAR>) <BODY>) <EXP>)
;;
;; Use the environment model to analyze this alternate version of
;; `make-withdraw', drawing figures like the ones above to illustrate
;; the interactions

(define W1 (make-withdraw 100))

(W1 50)

(define W2 (make-withdraw 100))

;; Show that the two versions of `make-withdraw' create objects with
;; the same behavior.  How do the environment structures differ for
;; the two versions?

;; See exercise.3.10.pdf

(define (make-withdraw1 balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-withdraw2 initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds"))) initial-amount))

;; the only difference between the two structures are the extra
;; environment created for make-withdraw2. However, since
;; initial-amount isn't used within the body of the inner-lambda,
;; they're entirely equivalent in every way.

;; (assert-equal x y)
(done)
