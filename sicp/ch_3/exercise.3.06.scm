
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.6

;; It is useful to be able to reset a random-number
;; generator to produce a sequence starting from a given value.
;; Design a new `rand' procedure that is called with an argument that
;; is either the symbol `generate' or the symbol `reset' and behaves
;; as follows: `(rand 'generate)' produces a new random number;
;; `((rand 'reset) <NEW-VALUE>)' resets the internal state variable
;; to the designated <NEW-VALUE>.  Thus, by resetting the state, one
;; can generate repeatable sequences.  These are very handy to have
;; when testing and debugging programs that use random numbers.

(define (rand-update x)
  ;; primes 100006, 100007, and 100008
  (let ((a 1299817) (b 1299821) (m 1299827) (% modulo))
    (% (+ (* a x) b) m)))

(define (make-rand)
  (let ((x -1))
    (lambda (msg)
      (define (reset n)
        (set! x n)
        x)
      (define (generate)
        (set! x (rand-update x))
        x)
      (cond ((eq? msg 'reset) reset)
            ((eq? msg 'generate) (generate))
            (else (error "Undefined message: " msg))))))

(define rand (make-rand))

(assert-equal 42                             ((rand 'reset) 42))
(assert-equal (rand-update 42)               (rand 'generate))
(assert-equal (rand-update (rand-update 42)) (rand 'generate))

(assert-equal 42                             ((rand 'reset) 42))
(assert-equal (rand-update 42)               (rand 'generate))
(done)
