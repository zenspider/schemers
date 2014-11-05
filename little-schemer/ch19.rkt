;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(require "../sicp/lib/test.rkt")
(module+ test (require rackunit))

(define (I x) x) ; just for testing

(define (deep m)
  (cond ((zero? m) 'pizza)
        (else (cons (deep (sub1 m)) '()))))

(define toppings #f)
(define (deepB m)
  (cond ((zero? m)
         (let/cc jump
           (set! toppings jump)
           'pizza))
        (else (cons (deepB (sub1 m)) '()))))

(test '((((((pizza)))))) (deep 6))

(define x (deepB 6))
(test '((((((pizza)))))) (I x))

(set! x (toppings 'mozzarella))
(test '((((((mozzarella)))))) (I x))

(set! x (toppings 'cake))
(test '((((((cake)))))) (I x))

(set! x (cons (toppings 'cake) '()))
(test '((((((cake)))))) (I x))

(define (deep&co m k)
  (cond ((zero? m) (k 'pizza))
        (else (deep&co (sub1 m) (lambda (x) (k (cons x '())))))))

(test 'pizza (deep&co 0 I))
(test '((((((pizza)))))) (deep&co 6 I))

(define (deep&coB m k)
  (cond ((zero? m) (let ()
                     (set! toppings k)
                     (k 'pizza)))
        (else (deep&coB (sub1 m) (lambda (x) (k (cons x '())))))))

(test '((((pizza)))) (deep&coB 4 I))

(set! x (cons (toppings 'cake) (toppings 'cake)))
(test '(((((cake)))) (((cake)))) (I x))

;;; 20th Commandment
;;
;; When thinking about a value created with (letcc ...), write down
;; the function that is equivalent but does not forget. Then, when you
;; use it, remember to forget.

(set! x (cons (toppings 'cake) (toppings 'cake)))
(test '(((((cake)))) (((cake)))) (I x))

(define leave #f)
(define (walk l)
  (cond ((null? l) '())
        ((atom? (car l)) (leave (car l)))
        (else (walk (car l))
              (walk (cdr l)))))

(define (start-it l)
  (let/cc here
    (set! leave here)
    (walk l)))

(set! x (start-it '((potato) (chips (chips (with))) fish)))
(test 'potato (I x))

(define two-in-a-row*?
  (letrec ((T?
            (lambda (a)
              (let ((n (get-next 0)))
                (if (atom? n)
                    (or (eq? n a) (T? n))
                    #f))))
           (get-next
            (lambda (x)
              (let/cc here-again
                (set! leave here-again)
                (fill 'go))))
           (fill
            (lambda (x) x))
           (waddle
            (lambda (l)
              (cond ((null? l) '())
                    ((atom? (car l))
                     (let/cc rest
                       (set! fill rest)
                       (leave (car l)))
                     (waddle (cdr l)))
                    (else
                     (waddle (car l))
                     (waddle (cdr l))))))
           (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                   (set! leave here)
                   (waddle l)
                   (leave '()))))
        (if (atom? fst) (T? fst) #f)))))

(set! x (two-in-a-row*? '(((food) ()) (((food))))))
(test #t (I x))
