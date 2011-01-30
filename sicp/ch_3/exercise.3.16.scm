
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.16

;; Ben Bitdiddle decides to write a procedure to
;; count the number of pairs in any list structure.  "It's easy," he
;; reasons.  "The number of pairs in any structure is the number in
;; the `car' plus the number in the `cdr' plus one more to count the
;; current pair."  So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct.  In particular, draw
;; box-and-pointer diagrams representing list structures made up of
;; exactly three pairs for which Ben's procedure would return 3;
;; return 4; return 7; never return at all.

(define a '(a))
(define b (cons a a))
(define c (cons b b))

(define x '(a b c))
(define y (cons 'b b))
(define z c)

;; 3 = *--*--*--0
;;     |  |  |
;;     a  b  c
;;
;;        +--+
;;        |  v
;; 4 = *--*--*--0
;;     |     |
;;     b     a
;;
;;        +--+
;;        |  v
;; 7 = *--*--*--0
;;     |  ^
;;     +--+


(assert-equal 3 (count-pairs x))
(assert-equal 4 (count-pairs y))
(assert-equal 7 (count-pairs z))

;; (assert-equal x y)
(done)
