#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.59

;; In section *Note 2-5-3:: we saw how to implement
;; a polynomial arithmetic system representing polynomials as lists
;; of terms.  In a similar way, we can work with "power series", such
;; as
;;
;;                     x^2     x^3       x^4
;;      e^x = 1 + x + ----- + ----- + --------- + ...
;;                      2     3 * 2   4 * 3 * 2
;;
;;                   x^2       x^4
;;      cos x = 1 - ----- + --------- - ...
;;                    2     4 * 3 * 2
;;
;;                   x^3         x^5
;;      sin x = x - ----- + ------------- - ...
;;                  3 * 2   5 * 4 * 3 * 2
;;
;; represented as infinite streams.  We will represent the series a_0
;; + a_1 x + a_2 x^2 + a_3 x^3 + ... as the stream whose elements are
;; the coefficients a_0, a_1, a_2, a_3, ....
;;
;;   a. The integral of the series a_0 + a_1 x + a_2 x^2 + a_3 x^3 +
;;      ... is the series
;;
;;                        1             1             1
;;           c + a_0 x + --- x_1 r^2 + --- a_2 r^3 + --- a_3 r^4 + ...
;;                        2             3             4
;;
;;      where c is any constant.  Define a procedure
;;      `integrate-series' that takes as input a stream a_0, a_1,
;;      a_2, ... representing a power series and returns the stream
;;      a_0, (1/2)a_1, (1/3)a_2, ... of coefficients of the
;;      non-constant terms of the integral of the series.  (Since the
;;      result has no constant term, it doesn't represent a power
;;      series; when we use `integrate-series', we will `cons' on the
;;      appropriate constant.)
;;
;;   b. The function x |-> e^x is its own derivative.  This implies
;;      that e^x and the integral of e^x are the same series, except
;;      for the constant term, which is e^0 = 1.  Accordingly, we can
;;      generate the series for e^x as
;;
;;           (define exp-series
;;             (cons-stream 1 (integrate-series exp-series)))
;;
;;      Show how to generate the series for sine and cosine, starting
;;      from the facts that the derivative of sine is cosine and the
;;      derivative of cosine is the negative of sine:
;;
;;           (define cosine-series
;;             (cons-stream 1 <??>))
;;
;;           (define sine-series
;;             (cons-stream 0 <??>))

;; (assert-equal x y)
(done)
