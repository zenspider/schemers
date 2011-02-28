(use test)
(require-library streams)
(import streams)
(use numbers)

;;; Exercise 3.68

;; Louis Reasoner thinks that building a stream of pairs from three
;; parts is unnecessarily complicated. Instead of separating the pair
;; (S_0,T_0) from the rest of the pairs in the first row, he proposes
;; to work with the whole first row, as follows:

(define (interleave s1 s2)
  (if (stream-null? s1) s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (pairs (stream-cdr s) (stream-cdr t))))

;; Does this work? Consider what happens if we evaluate `(pairs
;; integers integers)' using Louis's definition of `pairs'.

;; this causes an infinite loop... but I don't actually know why HELP
;; (define int-pairs (pairs integers integers))
