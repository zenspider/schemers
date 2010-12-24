#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;; Exercise 2.41

;; Write a procedure to find all ordered triples of distinct positive
;; integers i, j, and k less than or equal to a given integer n that
;; sum to a given integer s.

(define (sum l) (apply + l))

(define (enumerate-interval low high)
  (if (> low high) null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (wtf-do-you-call-this? n s)
  (filter (lambda (l) (= (sum l) s)) (unique-triples n)))

(assert-equal '((3 2 1) (4 2 1) (4 3 1) (4 3 2)) (unique-triples 4))
(assert-equal '()                (wtf-do-you-call-this? 4 10))
(assert-equal '((4 3 2))         (wtf-do-you-call-this? 4 9))
(assert-equal '((4 3 1) (5 2 1)) (wtf-do-you-call-this? 5 8))
(done)
