
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.52

;; Consider the sequence of expressions
;;
;;      (define sum 0)
;;
;;      (define (accum x)
;;        (set! sum (+ x sum))
;;        sum)
;;
;;      (define seq (stream-map accum (stream-enumerate-interval 1 20)))
;;      (define y (stream-filter even? seq))
;;      (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;;                               seq))
;;
;;      (stream-ref y 7)
;;
;;      (display-stream z)
;;
;; What is the value of `sum' after each of the above expressions is
;; evaluated?  What is the printed response to evaluating the
;; `stream-ref' and `display-stream' expressions?  Would these
;; responses differ if we had implemented `(delay <EXP>)' simply as
;; `(lambda () <EXP>)' without using the optimization provided by
;; `memo-proc'?  Explain

;; (assert-equal x y)
(done)
