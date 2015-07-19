#lang racket/base

(provide (all-defined-out))

(require "lib/reasonable.rkt")
(require "ch22.rkt")
(require "ch23.rkt")

(define (domain l dom)                  ; aka members°
  (cond-e [(null° l)]
          [(fresh (a d)
             (cons° a d l)
             (member° a dom)
             (domain d dom))]))

(define (safe b)
  (cond-a [(null° b) %s]
          [else (fresh (a d)
                  (cons° a d b)
                  (not° (member° a d))
                  (no-diagonals° a a d)
                  (safe d))]))

(define (not° g)
  (cond-a [g %u]
          [else %s]))

(define (sub1° n m)
  (cond-a [(null° n) (≈ n m)]
          [(fresh (_)
             (cons° m _ n))]))

(define (add1° n m)
  (cons° n '() m))

(define (no-diagonals° n m l)
  (cond-a [(null° l) %s]
          [else (fresh (a d n+1 n-1)
                  (sub1° n n-1)
                  (add1° m n+1)
                  (cons° a d l)
                  (cond-a [(≈ n+1 a) %u]
                          [(≈ n-1 a) %u]
                          [else (no-diagonals° n-1 n+1 d)]))]))

(define (n->l n)
  (cond [(zero? n) '()]
        [else (cons (n->l (sub1 n)) '())]))

(define (l->n l)
  (cond [(null? l) 0]
        [else (add1 (l->n (car l)))]))

(define (ns->ls l)   (map n->l l))      ; numbers to lists
(define (ls->ns l)   (map l->n l))      ; lists to numbers
(define (lls->lns l) (map ls->ns l))    ; lists of lists to lists of numbers
(define (lns-lls l)  (map ns->ls l))    ; lists of numbers to lists of lists

(require (for-syntax racket/base racket/list racket/syntax))

(define-syntax (queens stx)
  (syntax-case stx ()
    [(_ n b)
     (let ([n* (syntax->datum #'n)])
       (with-syntax* ([(names ...) (make-list n* 'q)]
                      [(num ...)   (range 1 (add1 n*))]
                      [(var ...)   (generate-temporaries #'(names ...))])
         #'(fresh (var ...)
               (all
                (≈ b (list var ...))

                (domain b (ns->ls '(num ...)))

                (safe b)))))]))

(module+ test
  (require rackunit)
  (require (submod "lib/reasonable.rkt" test))

  (check-equal? (n->l 0) '())
  (check-equal? (n->l 1) '(()))
  (check-equal? (n->l 2) '((())))
  (check-equal? (n->l 3) '(((()))))

  (check-equal? (l->n '())           0)
  (check-equal? (l->n '(()))         1)
  (check-equal? (l->n '((())))       2)
  (check-equal? (l->n '(((()))))     3)

  (check-run* (x) (sub1° (n->l 0) x) => (list (n->l 0)))
  (check-run* (x) (sub1° (n->l 1) x) => (list (n->l 0)))
  (check-run* (x) (sub1° (n->l 2) x) => (list (n->l 1)))

  (check-run* (x) (add1° (n->l 0) x) => (list (n->l 1)))
  (check-run* (x) (add1° (n->l 1) x) => (list (n->l 2)))
  (check-run* (x) (add1° (n->l 2) x) => (list (n->l 3)))

  (define-check (check-no-diagonals° nn ll exp)
    (check-run* (x)
                (fresh (n l)
                  (≈ n (n->l nn))
                  (≈ l (ns->ls ll))
                  (no-diagonals° n n l)
                  (≈ x #t))
                => exp))

  ;; good up
  (check-no-diagonals° 1 '()      good)
  (check-no-diagonals° 1 '(1)     good)
  (check-no-diagonals° 1 '(3)     good)
  (check-no-diagonals° 1 '(3 4 5) good)
  ;; good down
  (check-no-diagonals° 3 '(1)     good)
  (check-no-diagonals° 3 '(1 2 3) good)
  ;; bad up
  (check-no-diagonals° 1 '(2)     none)
  (check-no-diagonals° 1 '(4 3 1) none)
  ;; bad down
  (check-no-diagonals° 3 '(2)     none)
  (check-no-diagonals° 3 '(4 3 0) none)

  ;; known solution to 4x4
  (check-no-diagonals°    3 '(1 4 2) good)

  (define-check (check-safe l exp)
    (check-run* (x)
                (fresh (b)
                  (≈ b (ns->ls l))
                  (safe b)
                  (≈ x #t))
                => exp))

  (check-safe '(1)       good)
  (check-safe '(1 2)     none)          ; up
  (check-safe '(2 1)     none)          ; down
  (check-safe '(3 1)     good)
  (check-safe '(3 1 4 2) good)

  (time                          ; cpu time: 0 real time: 0 gc time: 0
   (check-run* (b)
               (queens 1 b)
               => (lns-lls '((1)))))

  (time                       ; cpu time: 5 real time: 5 gc time: 0
   (check-run* (b)
               (queens 4 b)
               => (lns-lls '((2 4 1 3)
                             (3 1 4 2)))))

  (time                   ; cpu time: 1353 real time: 1360 gc time: 21
   (check-run* (b)
               (queens 6 b)
               => (lns-lls '((2 4 6 1 3 5)
                             (3 6 2 5 1 4)
                             (4 1 5 2 6 3)
                             (5 3 1 6 4 2)))))

  (time             ; cpu time: 672954 real time: 672790 gc time: 5966
   (check-run* (b)
               (queens 8 b)
               => (lns-lls '((1 5 8 6 3 7 2 4)
                             (1 6 8 3 7 4 2 5)
                             (1 7 4 6 8 2 5 3)
                             (1 7 5 8 2 4 6 3)
                             (2 4 6 8 3 1 7 5)
                             (2 5 7 1 3 8 6 4)
                             (2 5 7 4 1 8 6 3)
                             (2 6 1 7 4 8 3 5)
                             (2 6 8 3 1 4 7 5)
                             (2 7 3 6 8 5 1 4)
                             (2 7 5 8 1 4 6 3)
                             (2 8 6 1 3 5 7 4)
                             (3 1 7 5 8 2 4 6)
                             (3 5 2 8 1 7 4 6)
                             (3 5 2 8 6 4 7 1)
                             (3 5 7 1 4 2 8 6)
                             (3 5 8 4 1 7 2 6)
                             (3 6 2 5 8 1 7 4)
                             (3 6 2 7 1 4 8 5)
                             (3 6 2 7 5 1 8 4)
                             (3 6 4 1 8 5 7 2)
                             (3 6 4 2 8 5 7 1)
                             (3 6 8 1 4 7 5 2)
                             (3 6 8 1 5 7 2 4)
                             (3 6 8 2 4 1 7 5)
                             (3 7 2 8 5 1 4 6)
                             (3 7 2 8 6 4 1 5)
                             (3 8 4 7 1 6 2 5)
                             (4 1 5 8 2 7 3 6)
                             (4 1 5 8 6 3 7 2)
                             (4 2 5 8 6 1 3 7)
                             (4 2 7 3 6 8 1 5)
                             (4 2 7 3 6 8 5 1)
                             (4 2 7 5 1 8 6 3)
                             (4 2 8 5 7 1 3 6)
                             (4 2 8 6 1 3 5 7)
                             (4 6 1 5 2 8 3 7)
                             (4 6 8 2 7 1 3 5)
                             (4 6 8 3 1 7 5 2)
                             (4 7 1 8 5 2 6 3)
                             (4 7 3 8 2 5 1 6)
                             (4 7 5 2 6 1 3 8)
                             (4 7 5 3 1 6 8 2)
                             (4 8 1 3 6 2 7 5)
                             (4 8 1 5 7 2 6 3)
                             (4 8 5 3 1 7 2 6)
                             (5 1 4 6 8 2 7 3)
                             (5 1 8 4 2 7 3 6)
                             (5 1 8 6 3 7 2 4)
                             (5 2 4 6 8 3 1 7)
                             (5 2 4 7 3 8 6 1)
                             (5 2 6 1 7 4 8 3)
                             (5 2 8 1 4 7 3 6)
                             (5 3 1 6 8 2 4 7)
                             (5 3 1 7 2 8 6 4)
                             (5 3 8 4 7 1 6 2)
                             (5 7 1 3 8 6 4 2)
                             (5 7 1 4 2 8 6 3)
                             (5 7 2 4 8 1 3 6)
                             (5 7 2 6 3 1 4 8)
                             (5 7 2 6 3 1 8 4)
                             (5 7 4 1 3 8 6 2)
                             (5 8 4 1 3 6 2 7)
                             (5 8 4 1 7 2 6 3)
                             (6 1 5 2 8 3 7 4)
                             (6 2 7 1 3 5 8 4)
                             (6 2 7 1 4 8 5 3)
                             (6 3 1 7 5 8 2 4)
                             (6 3 1 8 4 2 7 5)
                             (6 3 1 8 5 2 4 7)
                             (6 3 5 7 1 4 2 8)
                             (6 3 5 8 1 4 2 7)
                             (6 3 7 2 4 8 1 5)
                             (6 3 7 2 8 5 1 4)
                             (6 3 7 4 1 8 2 5)
                             (6 4 1 5 8 2 7 3)
                             (6 4 2 8 5 7 1 3)
                             (6 4 7 1 3 5 2 8)
                             (6 4 7 1 8 2 5 3)
                             (6 8 2 4 1 7 5 3)
                             (7 1 3 8 6 4 2 5)
                             (7 2 4 1 8 5 3 6)
                             (7 2 6 3 1 4 8 5)
                             (7 3 1 6 8 5 2 4)
                             (7 3 8 2 5 1 6 4)
                             (7 4 2 5 8 1 3 6)
                             (7 4 2 8 6 1 3 5)
                             (7 5 3 1 6 8 2 4)
                             (8 2 4 1 7 5 3 6)
                             (8 2 5 3 1 7 4 6)
                             (8 3 1 6 2 5 7 4)
                             (8 4 1 3 6 2 7 5)))))

  'done
  )
