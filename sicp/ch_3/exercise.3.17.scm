#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 3.17

;; Devise a correct version of the `count-pairs'
;; procedure of *Note Exercise 3-16:: that returns the number of
;; distinct pairs in any structure.  (Hint: Traverse the structure,
;; maintaining an auxiliary data structure that is used to keep track
;; of which pairs have already been counted.)

(define (count-pairs x)
  (let ((visited '()))
    (define (iterate l)
      (define (count f)
        (let ((cell (f l)))
          (if (or (memq cell visited) (not (pair? cell))) 0
              (begin
                (set! visited (cons cell visited))
                (+ 1 (iterate cell))))))
      (if (null? l) 0
          (+ (count car) (count cdr))))
    (+ 1 (iterate x))))

(define a '(a))
(define b (cons a a))
(define c (cons b b))

(define x '(a b c))
(define y (cons 'b b))
(define z c)

(test-group "3.17"
            (test 1 (count-pairs '()))
            (test 3 (count-pairs x))
            (test 3 (count-pairs y))
            (test 3 (count-pairs z)))
