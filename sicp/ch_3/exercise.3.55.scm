(require-library streams)
(import streams)
(use test)

;;; Exercise 3.55

;; Define a procedure `partial-sums' that takes as
;; argument a stream S and returns the stream whose elements are S_0,
;; S_0 + S_1, S_0 + S_1 + S_2, ....  For example, `(partial-sums
;; integers)' should be the stream 1, 3, 6, 10, 15, ....

(define ones     (stream-cons 1 ones))
(define integers (stream-cons 1 (stream-add ones integers)))

(define (old-partial-sums s)
  (define sums
    (stream-cons (stream-car s)
                 (stream-add sums
                             (stream-cdr s))))
  sums)

(define (partial-sums s)
  (stream-cons (stream-car s)
               (stream-add (partial-sums s)
                           (stream-cdr s))))

(test-group "3.55"
            (let ((numbers (partial-sums integers)))
              (test (+ 1)         (stream-ref numbers 0))
              (test (+ 1 2)       (stream-ref numbers 1))
              (test (+ 1 2 3)     (stream-ref numbers 2))
              (test (+ 1 2 3 4)   (stream-ref numbers 3))
              (test (+ 1 2 3 4 5) (stream-ref numbers 4))))
