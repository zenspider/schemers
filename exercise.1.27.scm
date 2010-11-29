#lang r5rs

;;; Exercise 1.27:

;; Demonstrate that the Carmichael numbers listed in *Note Footnote
;; 1-47:: really do fool the Fermat test. That is, write a procedure
;; that takes an integer n and tests whether a^n is congruent to a
;; modulo n for every a<n, and try your procedure on the given
;; Carmichael numbers.

;; [Footnote 1.47] Numbers that fool the Fermat test are called
;; "Carmichael numbers", and little is known about them other than
;; that they are extremely rare. There are 255 Carmichael numbers
;; below 100,000,000. The smallest few are 561, 1105, 1729, 2465,
;; 2821, and 6601. In testing primality of very large numbers chosen
;; at random, the chance of stumbling upon a value that fools the
;; Fermat test is less than the chance that cosmic radiation will
;; cause the computer to make an error in carrying out a "correct"
;; algorithm. Considering an algorithm to be inadequate for the first
;; reason but not for the second illustrates the difference between
;; mathematics and engineering.

(define (expmod base exp m)
  (remainder (expt base exp) m))

(define (fermat-test n)
  (define (iterator n a)
    (or (= a 0) (and (= (expmod a n n) a) (iterator n (- a 1)))))
  (iterator n (- n 1)))

(fermat-test 3)                         ; #t
(fermat-test 4)                         ; #f
(fermat-test 97)                        ; #t
(fermat-test 100)                       ; #f

;; commenting out because they're slow as fuck (22 seconds)
;; (fermat-test 561)                       ; #t
;; (fermat-test 1105)                      ; #t
;; (fermat-test 1729)                      ; #t
;; (fermat-test 2465)                      ; #t
;; (fermat-test 2821)                      ; #t
;; (fermat-test 6601)                      ; #t


