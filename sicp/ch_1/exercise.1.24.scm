
;;; Exercise 1.24:

;; Modify the `timed-prime-test' procedure of *Note Exercise 1-22:: to
;; use `fast-prime?' (the Fermat method), and test each of the 12
;; primes you found in that exercise. Since the Fermat test has
;; [theta](`log' n) growth, how would you expect the time to test
;; primes near 1,000,000 to compare with the time needed to test
;; primes near 1000? Do your data bear this out? Can you explain any
;; discrepancy you find?

(define (square n) (* n n))

(define (prime? n times)
  (define (fermat-test n)
    (define (try-it a)
      (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
              (else        (remainder (* base (expmod base (- exp 1) m)) m))))
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) #t)
        ((fermat-test n) (prime? n (- times 1)))
        (else #f)))

(define runtime current-milliseconds)

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (and (prime? n 5)
       (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display " = ")
  (display elapsed-time)
  (display " ms")
  (newline))

(define (!= a b) (not (= a b)))

(define (search-for-primes from count)
  (and (!= count 0)
       (if (timed-prime-test from)
           (search-for-primes (+ from 1) (- count 1))
           (search-for-primes (+ from 1) count))))

(search-for-primes 1001 3)

;; old
;; 1009    = 0.0009765625 ms
;; 1013    = 0.0009765625 ms
;; 1019    = 0.0009765625 ms

;; new
;; 1009    = 0.0009765625 ms
;; 1013    = 0.0009765625 ms
;; 1019    = 0.0009765625 ms

;; fermat
;; 1009    = 0.329833984375 ms
;; 1013    = 0.006103515625 ms
;; 1019    = 0.006103515625 ms

(search-for-primes 10001 3)

;; old
;; 10007   = 0.001953125 ms
;; 10009   = 0.001953125 ms
;; 10037   = 0.001953125 ms

;; new
;; 10007   = 0.0009765625 ms
;; 10009   = 0.001953125 ms
;; 10037   = 0.0009765625 ms

;; fermat
;; 10007   = 0.008056640625 ms
;; 10009   = 0.008056640625 ms
;; 10037   = 0.008056640625 ms

(search-for-primes 100001 3)

;; old
;; 100003  = 0.0068359375 ms
;; 100019  = 0.007080078125 ms
;; 100043  = 0.005859375 ms

;; new
;; 100003  = 0.002197265625 ms
;; 100019  = 0.0029296875 ms
;; 100043  = 0.0029296875 ms

;; fermat
;; 100003  = 0.047119140625 ms
;; 100019  = 0.0439453125 ms
;; 100043  = 0.0458984375 ms

(search-for-primes 1000001 3)

;; old
;; 1000003 = 0.018798828125 ms
;; 1000033 = 0.018798828125 ms
;; 1000037 = 0.019775390625 ms

;; new
;; 1000003 = 0.0078125 ms
;; 1000033 = 0.007080078125 ms
;; 1000037 = 0.008056640625 ms

;; fermat
;; 1000003 = 0.06103515625 ms
;; 1000033 = 0.06689453125 ms
;; 1000037 = 0.06298828125 ms

;; slower across the board... my guess is that racket is that much
;; slower, or it is a bad rand algorithm or... something.
