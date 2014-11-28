#lang racket/base

(require "../lib/test.rkt")

;;; Exercise 3.20

;; Draw environment diagrams to illustrate the
;; evaluation of the sequence of expressions
;;
;;      (define x (cons 1 2))
;;      (define z (cons x x))
;;      (set-car! (cdr z) 17)
;;
;;      (car x)
;;      17
;;
;; using the procedural implementation of pairs given above.  (Compare
;; *Note Exercise 3-11::.)

(define (xcons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (xcar z) (z 'car))

(define (xcdr z) (z 'cdr))

(define (xset-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (xset-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; E0: xcons, xcar, xcdr, xset-car!, xset-cdr!

(define x (xcons 1 2))

;; E0: xcons, xcar, xcdr, xset-car!, xset-cdr!: ...code...
;;     x -> E1
;; E1: set-x! set-y! dispatch: ...code...
;;     x: 1
;;     y: 2

(define z (xcons x x))

;; E0: xcons, xcar, xcdr, xset-car!, xset-cdr!: ...code...
;;     x -> E1
;;     z -> E2
;; E1: set-x! set-y! dispatch: ...code...
;;     x: 1
;;     y: 2
;; E2: set-x! set-y! dispatch: ...code...
;;     x -> E0:x -> E1
;;     y -> E0:x -> E1

(xset-car! (xcdr z) 17)

;; (xcdr z) => ((z 'cdr)) => E2:y => E0:x aka x
;; (xset-car! x 17) => ((z 'set-x!) 17) => (set! E1:x 17)

;; E1: set-x! set-y! dispatch: ...code...
;;     x: 17
;;     y: 2

(test 17 (xcar x))

;; (xcar x) => ((z 'car)) => E1:x => 17
