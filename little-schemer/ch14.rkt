#lang racket/base

(require "../sicp/lib/test.rkt")
(module+ test (require rackunit))

(define-me-maybe atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define test-leftmost
  (lambda (f desc)
    (test-group desc
     (test 'a (f '(((a) b) (c d))))
     (test 'a (f '(((() a) ())))))))

(define leftmost1
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (car l))
          (else (let ((a (leftmost1 (car l))))
                  (cond ((atom? a) a)
                        (else (leftmost1 (cdr l)))))))))

(test-leftmost leftmost1 "leftmost1")

;;; 15th Commandment (preliminary)
;;
;; Use (let ...) to name the values of repeated expressions.

(define depth1*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth1* (cdr l)))
          (else
           (let ((a (add1 (depth1* (car l))))
                 (d (depth1* (cdr l))))
             (cond
              ((> d a) d)
              (else a)))))))

(test 2 (depth1* '((a) b (c d))))

;;; 15th Commandment (revised)
;;
;; Use (let ...) to name the values of repeated expressions in a
;; function definition if they may be evaluated twice for one and the
;; same use of the function.

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth2*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth2* (cdr l)))
          (else
           (max (add1 (depth2* (car l)))
                (depth2* (cdr l)))))))

(define leftmost2
  (lambda (l)
    (let/cc skip
      (letrec ((lm (lambda (l)
                     (cond ((null? l) '())
                           ((atom? (car l)) (skip (car l)))
                           (else (begin
                                   (lm (car l))
                                   (lm (cdr l))))))))
        (lm l)))))

(test-leftmost leftmost2 "leftmost-callcc")

(define-syntax try
  (syntax-rules ()
    ((_ x a b) (let/cc success
                 (let/cc x
                   (success a))
                 b))))

;; (define try
;;   (lambda (x a b)
;;     (let/cc success
;;       (let/cc x
;;         (success a))
;;       b)))
;;
;; (try )

(define rm1
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm1 a (cdr l) oh))))
          (else
           (let ((new-car (let/cc oh (rm1 a (car l) oh))))
             (if (atom? new-car)
                 (cons (car l) (rm1 a (cdr l) oh))
                 (cons new-car (cdr l))))))))

(define rember1*
  (lambda (a l)
    (let ((new-l (let/cc oh (rm1 a l oh))))
      (if (atom? new-l)
          l
          new-l))))

(test 'no (let/cc Say (rm1 'noodles '((food) more (food)) Say)))
(test '((food) more (food)) (rember1* 'noodles '((food) more (food))))

(define rember2*
  (lambda (a l)
    (try oh (rm1 a l oh) l)))

(test '((food) more (food)) (rember2* 'noodles '((food) more (food))))

(define rm2
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm2 a (cdr l) oh))))
          (else
           (try oh2
                (cons (rm2 a (car l) oh2) (cdr l))
                (cons (car l) (rm2 a (cdr l) oh)))))))

(test '((food) more (food)) (rember2* 'noodles '((food) more (food))))
