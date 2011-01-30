
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.12

;; The following procedure for appending lists was introduced in
;; section *Note 2-2-1:::

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;; `Append' forms a new list by successively `cons'ing the elements of
;; `x' onto `y'. The procedure `append!' is similar to `append', but
;; it is a mutator rather than a constructor. It appends the lists by
;; splicing them together, modifying the final pair of `x' so that its
;; `cdr' is now `y'. (It is an error to call `append!' with an empty
;; `x'.)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;; Consider the interaction

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(assert-equal '(a b c d) z)
(assert-equal '(b) (cdr x))

;; x = *--*--0
;;     |  |
;;     a  b
;;
;; (cdr x) = *--0
;;           |
;;           b

(define w (append! x y))
(assert-equal '(a b c d) w)
(assert-equal '(b c d) (cdr x))

;; x = *--*--0
;;     |  |
;;     a  b
;;
;; y = *--*--0
;;     |  |
;;     c  d
;;
;; (append! x y)
;; (set-cdr! (last-pair x) y)
;; (set-cdr! (cdr x) y)
;;
;; x = *--*--*--*--0
;;     |  |  |  |
;;     a  b  c  d
;;
;; (cdr x) = *--*--*--*--0
;;           |  |  |  |
;;           a  b  c  d

(done)
