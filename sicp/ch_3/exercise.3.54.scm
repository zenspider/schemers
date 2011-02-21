
(require-library streams)
(import streams)
(use test)

;;; Exercise 3.54

;; Define a procedure `mul-streams', analogous to
;; `add-streams', that produces the elementwise product of its two
;; input streams.  Use this together with the stream of `integers' to
;; complete the following definition of the stream whose nth element
;; (counting from 0) is n + 1 factorial:
;;
;;      (define factorials (cons-stream 1 (mul-streams <??> <??>)))

(define (stream-mul s1 s2)
  (stream-map * s1 s2))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

;; HELP: why doesn't this work?
;; (define ones     (stream-cons 1 ones))
;; (define integers (stream-cons 1 (stream-add ones integers)))
;; (define factorials
;;   (stream-mul integers (stream-cdr factorials)))

(define factorials
  (stream-cons 1
               (stream-cons 2
                            (stream-mul (integers-starting-from 3)
                                        (stream-cdr factorials)))))

(test-group "3.54"
            (test '(1 2 6 24 120) (stream-head factorials 5))
            (test (* 1)         (stream-ref factorials 0))
            (test (* 1 2)       (stream-ref factorials 1))
            (test (* 1 2 3)     (stream-ref factorials 2))
            (test (* 1 2 3 4)   (stream-ref factorials 3))
            (test (* 1 2 3 4 5) (stream-ref factorials 4)))
