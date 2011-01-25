
;;; *Exercise 1.7:*

;; The `good-enough?' test used in computing square roots will not be
;; very effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always
;; performed with limited precision. This makes our test inadequate
;; for very large numbers. Explain these statements, with examples
;; showing how the test fails for small and large numbers. An
;; alternative strategy for implementing `good-enough?' is to watch
;; how `guess' changes from one iteration to the next and to stop when
;; the change is a very small fraction of the guess. Design a
;; square-root procedure that uses this kind of end test. Does this
;; work better for small and large numbers?

;; shared definitions

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

;; initial version

(define (improve1 guess x)
  (average guess (/ x guess)))

(define (good-enough1? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter1 guess x)
  (if (good-enough1? guess x)
      guess
      (sqrt-iter1 (improve1 guess x) x)))

(define (sqrt1 x)
  (sqrt-iter1 1.0 x))

(sqrt1 9)                               ;    3.00009155413138
(sqrt1 (+ 100 37))                      ;   11.704699917758145
(sqrt1 (+ (sqrt1 2) (sqrt1 3)))         ;    1.7739279023207892
(square (sqrt1 1000))                   ; 1000.000369924366

;; proportional version

(define (good-enough2? guess x)
  (< (abs (- (square guess) x)) (/ x 1000000)))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
      guess
      (sqrt-iter2 (improve1 guess x) x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

(sqrt2 9)                                ;    3.000000001396984
(sqrt2 (+ 100 37))                       ;   11.704699917758145
(sqrt2 (+ (sqrt2 2) (sqrt2 3)))          ;    1.7737712336472033
(square (sqrt1 1000))                    ; 1000.000369924366

(define (diff-pct a b) (/ (- a b) a))

(diff-pct 3.00009155413138 3.000000001396984)    ; 3.0516646823741233e-05
(diff-pct 11.704699917758145 11.704699917758145) ; 0.0
(diff-pct 1.7739279023207892 1.7737712336472033) ; 8.831738504192543e-05
(diff-pct 1000.000369924366 1000.000369924366)   ; 0.0

;; changing good-enough? to be proportional to the original x seems to
;; only improve the smallest numbers by a marginal amount.

;; feedback version

(define (improve x guess)
  (average guess (/ x guess)))

(define (good-enough? x guess old-guess)
  (< (abs (- old-guess guess)) (/ guess 1000000)))

(define (sqrt-iter x guess old-guess)
  (if (good-enough? x guess old-guess)
      guess
      (sqrt-iter x (improve x guess) guess)))

(define (sqrt x)
  (sqrt-iter x 1.0 0.0))

(sqrt 9)                                ; 3.0
(sqrt (+ 100 37))                       ; 11.704699910719626
(sqrt (+ (sqrt 2) (sqrt 3)))            ; 1.773771228186423
(square (sqrt 1000))                    ; 1000.0000000000343

(diff-pct 3.00009155413138 3.0)                  ; 3.0517112470817903e-05
(diff-pct 11.704699917758145 11.704699910719626) ; 6.01341268921337e-10
(diff-pct 1.7739279023207892 1.773771228186423)  ; 8.83204633971761e-05
(diff-pct 1000.000369924366 1000.0000000000343)  ; 3.699241948774763e-07

;; this version makes good-enough test proportional to guess as well
;; as testing guess against old-guess instead of the square of guess
;; and x. This improvement leads to slight improvements on the larger
;; numbers as well as very similar improvements on smaller numbers.
