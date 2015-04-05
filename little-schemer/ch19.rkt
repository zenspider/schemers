;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit)
  (require (only-in racket/function identity))

  (define x #f))

(require "lib/shared.rkt")

(define (deep m)
  (cond [(zero? m) 'pizza]
        [else (cons (deep (sub1 m)) '())]))

(define toppings #f)
(define (deepB m)
  (cond [(zero? m)
         (let/cc jump
                 (set! toppings jump)
                 'pizza)]
        [else (cons (deepB (sub1 m)) '())]))

(module+ test
  (set! x (deep 6))
  (check-equal? x '((((((pizza)))))))

  (set! x (deepB 6))
  (check-equal? x '((((((pizza)))))))

  (set! x (toppings 'mozzarella))
  (check-equal? x '((((((mozzarella)))))))

  (set! x (toppings 'cake))
  (check-equal? x '((((((cake)))))))

  (set! x (cons (toppings 'cake) '()))
  (check-equal? x '((((((cake))))))))

(define (deep&co m k)
  (cond [(zero? m) (k 'pizza)]
        [else (deep&co (sub1 m) (lambda (x) (k (cons x '()))))]))

(module+ test
  (check-equal? (deep&co 0 identity) 'pizza)
  (check-equal? (deep&co 6 identity) '((((((pizza))))))))

(define (deep&coB m k)
  (cond [(zero? m) (let ()
                     (set! toppings k)
                     (k 'pizza))]
        [else (deep&coB (sub1 m) (lambda (x) (k (cons x '()))))]))

(module+ test
  (set! x (deep&coB 4 identity))
  (check-equal? x '((((pizza)))))

  (set! x (cons (toppings 'cake) (toppings 'cake)))
  (check-equal? x '(((((cake)))) (((cake)))))

  (set! x (cons (toppings 'cake) (toppings 'cake)))
  (check-equal? x '(((((cake)))) (((cake))))))

(define leave #f)
(define (walk l)
  (cond [(null? l) '()]
        [(atom? (car l)) (leave (car l))]
        [else (walk (car l))
              (walk (cdr l))]))

(define (start-it l)
  (let/cc here
          (set! leave here)
          (walk l)))

(module+ test
  (set! x (start-it '((potato) (chips (chips (with))) fish)))

  (check-equal? x 'potato))

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
              (cond [(null? l) '()]
                    [(atom? (car l))
                     (let/cc rest
                             (set! fill rest)
                             (leave (car l)))
                     (waddle (cdr l))]
                    [else
                     (waddle (car l))
                     (waddle (cdr l))])))
           (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                         (set! leave here)
                         (waddle l)
                         (leave '()))))
        (if (atom? fst) (T? fst) #f)))))

(module+ test
  (set! x (two-in-a-row*? '(((food) ()) (((food))))))
  (check-true x))
