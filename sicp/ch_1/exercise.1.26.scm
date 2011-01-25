
;;; Exercise 1.26:

;; Louis Reasoner is having great difficulty doing *Note Exercise
;; 1-24::. His `fast-prime?' test seems to run more slowly than his
;; `prime?' test. Louis calls his friend Eva Lu Ator over to help.
;; When they examine Louis's code, they find that he has rewritten the
;; `expmod' procedure to use an explicit multiplication, rather than
;; calling `square':

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;; "I don't see what difference that could make," says Louis. "I do."
;; says Eva. "By writing the procedure like that, you have transformed
;; the [theta](`log' n) process into a [theta](n) process." Explain.


;; When the exponent is even, he's performing twice the number of
;; recursive expmod calls, eliminating any benefit to begin with. This
;; transforms the problem from O(log n) to O(n).
;;
;; Still not sure why they wouldn't use the built in expt... I'm fairly
;; certain it was standard even when this book was written. :P
