(use test)
(require-library streams)
(import streams)
(use numbers)

;;; Exercise 3.66

;; Examine the stream `(pairs integers integers)'.
;; Can you make any general comments about the order in which the
;; pairs are placed into the stream? For example, about how many
;; pairs precede the pair (1,100)?  the pair (99,100)? the pair
;; (100,100)? (If you can make precise mathematical statements here,
;; all the better. But feel free to give more qualitative answers if
;; you find yourself getting bogged down.)

(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons (list (stream-car s) (stream-car t))
               (interleave (stream-map (lambda (x) (list (stream-car s) x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

;; A: every other item (except the first) is '(1 n), so '(1 100) would
;; be (- (* 2 99) 1) elements in:

(test '(1 100) (stream-ref int-pairs 197))

;; each (n n) is at (+ 2^1 2^2 ... 2^(n-1))

(define (sum-of-powers-of-2 n)
  (apply + (map (lambda (x) (expt 2 x)) (stream-head integers n))))

(test '(1 1) (stream-ref int-pairs (sum-of-powers-of-2 (- 1 1))))
(test '(2 2) (stream-ref int-pairs (sum-of-powers-of-2 (- 2 1))))
(test '(3 3) (stream-ref int-pairs (sum-of-powers-of-2 (- 3 1))))
(test '(4 4) (stream-ref int-pairs (sum-of-powers-of-2 (- 4 1))))
(test '(5 5) (stream-ref int-pairs (sum-of-powers-of-2 (- 5 1))))

;; so:
;; (100 100) is at (+ 2^1 2^2 2^3 ... 2^99)
;; or (sum-of-powers-of-2 (- 100 1))
;; or 1267650600228229401496703205374

;; (stream-head int-pairs 200)
