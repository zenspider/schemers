(require-library streams)
(import streams)
(use test)

;;; Exercise 3.53

;; Without running the program, describe the elements of the stream defined by:

(define s (stream-cons 1 (stream-add s s)))

;; s is a doubler: 1 2 4 8 16 ...

(test 1024 (stream-ref s 10))
