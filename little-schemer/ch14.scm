#!/usr/local/bin/csi -s

(use test)
(use miscmacros)                        ; call/cc

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define test-leftmost
  (lambda (f desc)
    (test-group desc
     (test 'a (f '(((a) b) (c d))))
     (test 'a (f '(((() a) ())))))))

(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (car l))
          (else (let ((a (leftmost (car l))))
                  (cond ((atom? a) a)
                        (else (leftmost (cdr l)))))))))

(test-leftmost leftmost "leftmost1")

;;; 15th Commandment (preliminary)
;;
;; Use (let ...) to name the values of repeated expressions.

(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth* (cdr l)))
          (else
           (let ((a (add1 (depth* (car l))))
                 (d (depth* (cdr l))))
             (cond
              ((> d a) d)
              (else a)))))))

(test 2 (depth* '((a) b (c d))))

;;; 15th Commandment (revised)
;;
;; Use (let ...) to name the values of repeated expressions in a
;; function definition if they may be evaluated twice for one and the
;; same use of the function.

(define max
  (lambda (n m)
    (if (> n m) n m)))

(define depth*
  (lambda (l)
    (cond ((null? l) 1)
          ((atom? (car l)) (depth* (cdr l)))
          (else
           (max (add1 (depth* (car l)))
                (depth* (cdr l)))))))

(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec ((lm (lambda (l)
                     (cond ((null? l) '())
                           ((atom? (car l)) (skip (car l)))
                           (else (begin
                                   (lm (car l))
                                   (lm (cdr l))))))))
        (lm l)))))

(test-leftmost leftmost "leftmost-callcc")

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

(define rm
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm a (cdr l) oh))))
          (else
           (let ((new-car (let/cc oh (rm a (car l) oh))))
             (if (atom? new-car)
                 (cons (car l) (rm a (cdr l) oh))
                 (cons new-car (cdr l))))))))

(define rember1*
  (lambda (a l)
    (let ((new-l (let/cc oh (rm a l oh))))
      (if (atom? new-l)
          l
          new-l))))

(test 'no (let/cc Say (rm 'noodles '((food) more (food)) Say)))
(test '((food) more (food)) (rember1* 'noodles '((food) more (food))))

(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))

(test '((food) more (food)) (rember1* 'noodles '((food) more (food))))

(define rm
  (lambda (a l oh)
    (cond ((null? l) (oh 'no))
          ((atom? (car l)) (if (eq? (car l) a)
                               (cdr l)
                               (cons (car l) (rm a (cdr l) oh))))
          (else
           (try oh2
                (cons (rm a (car l) oh2) (cdr l))
                (cons (car l) (rm a (cdr l) oh)))))))

(test '((food) more (food)) (rember1* 'noodles '((food) more (food))))
