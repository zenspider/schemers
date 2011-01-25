
;;; Exercise 1.17:

;; The exponentiation algorithms in this section are based on
;; performing exponentiation by means of repeated multiplication. In a
;; similar way, one can perform integer multiplication by means of
;; repeated addition. The following multiplication procedure (in which
;; it is assumed that our language can only add, not multiply) is
;; analogous to the `expt' procedure:

;; (define (* a b)
;;   (if (= b 0) 0
;;       (+ a (* a (- b 1)))))

;; This algorithm takes a number of steps that is linear in `b'. Now
;; suppose we include, together with addition, operations `double',
;; which doubles an integer, and `halve', which divides an (even)
;; integer by 2. Using these, design a multiplication procedure
;; analogous to `fast-expt' that uses a logarithmic number of steps.

(define (double n) (+ n n))
(define (halve  n) (/ n 2))             ; that makes no sense...

;; (* 11 10)
;; (double (* 11 5))
;; (double (+ 11 (* 11 4)))
;; (double (+ 11 (double (* 11 2))))
;; (double (+ 11 (double (double 11))))

(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult a (halve b))))
        (else      (+ a    (mult a (- b 1))))))

(mult 11 10)                            ; 110
(mult 65 64)                            ; 4160
(mult 3 127)                            ; 381
