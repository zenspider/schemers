
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 2.54:

;; Two lists are said to be `equal?' if they contain equal elements
;; arranged in the same order. For example,
;;
;;      (equal? '(this is a list) '(this is a list))
;;
;; is true, but
;;
;;      (equal? '(this is a list) '(this (is a) list))
;;
;; is false.  To be more precise, we can define `equal?'  recursively
;; in terms of the basic `eq?' equality of symbols by saying that `a'
;; and `b' are `equal?' if they are both symbols and the symbols are
;; `eq?', or if they are both lists such that `(car a)' is `equal?'
;; to `(car b)' and `(cdr a)' is `equal?' to `(cdr b)'.  Using this
;; idea, implement `equal?' as a procedure.(5)

(define (myequal? a b)
  (or (and (pair? a)
           (pair? b)
           (myequal? (car a) (car b))
           (myequal? (cdr a) (cdr b)))
      (eq? a b)))

(assert-equal #t (myequal? 'x 'x))
(assert-equal #t (myequal? null null))
(assert-equal #t (myequal? '(x) '(x)))
(assert-equal #t (myequal? '(((x)) y) '(((x)) y)))

(assert-equal #f (myequal? '(x) '(y)))
(assert-equal #f (myequal? '(x) 'x))
(assert-equal #f (myequal? 'x '(x)))
(assert-equal #f (myequal? 'x 'y))
(done)
