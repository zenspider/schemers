#lang racket/base

(require (except-in "../lib/streams.scm" ones integers))
(require "../lib/test.rkt")

;;; Exercise 3.56

;; A famous problem, first raised by R. Hamming, is
;; to enumerate, in ascending order with no repetitions, all positive
;; integers with no prime factors other than 2, 3, or 5.  One obvious
;; way to do this is to simply test each integer in turn to see
;; whether it has any factors other than 2, 3, and 5.  But this is
;; very inefficient, since, as the integers get larger, fewer and
;; fewer of them fit the requirement.  As an alternative, let us call
;; the required stream of numbers `S' and notice the following facts
;; about it.
;;
;;    * `S' begins with 1.
;;    * The elements of `(scale-stream S 2)' are also elements of `S'.
;;    * The same is true for `(scale-stream S 3)' and `(scale-stream 5 S)'.
;;    * These are all the elements of `S'.
;;
;; Now all we have to do is combine elements from these sources.  For
;; this we define a procedure `merge' that combines two ordered
;; streams into one ordered result stream, eliminating repetitions:

(define (stream-merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (stream-merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (stream-merge  s1 (stream-cdr s2))))
                 (else
                  (stream-cons s1car
                               (stream-merge (stream-cdr s1)
                                             (stream-cdr s2)))))))))

;; Then the required stream may be constructed with `stream-merge', as
;; follows:
;;
;;      (define S (stream-cons 1 (stream-merge <??> <??>)))
;;
;; Fill in the missing expressions in the places marked <??> above.

;; todo maybe: I think it'd be cleaner to produce: 2^i·3^j·5^k : [ijk] >= 0
(define S (stream-cons 1 (stream-merge (stream-scale S 2)
                                       (stream-merge (stream-scale S 3)
                                                     (stream-scale S 5)))))

;; Show the 1691st Hamming number (the last one below 231).

(test-group "3.56"
            (test '(1 2 3 4 5 6 8 9 10 12 15 16 18 20
                      24 25 27 30 32 36 40 45 48 50 54 60)
                  (stream-head S 26))
            (test 2125764000 (stream-ref S 1690)))
