(require "../lib/test.rkt")

;; Exercise 2.30.

;; Define a procedure square-tree analogous to the square-list
;; procedure of exercise 2.21. That is, square-list should behave as
;; follows:

;; (square-tree '(1 (2 (3 4) 5) (6 7)))
;; => (1 (4 (9 16) 25) (36 49))

;; Define square-tree both directly (i.e., without using any
;; higher-order procedures) and also by using map and recursion.

(define (square n) (* n n))

(define (square-tree1 l)
  (define (iterate l r)
    (if (null? l) r
        (iterate (cdr l)
                 (append r (list ((if (list? (car l)) square-tree1 square)
                                  (car l)))))))
  (iterate l '()))

(define (square-tree2 l)
  (cond ((null? l) l)
        ((list? (car l))
         (cons (square-tree2 (car l)) (square-tree2 (cdr l))))
        (else
         (cons (square (car l))       (square-tree2 (cdr l))))))

(define (square-tree3 l)
  (if (null? l) l
      (cons ((if (list? (car l)) square-tree3 square) (car l))
            (square-tree3 (cdr l)))))

(define (square-tree4 l)
  (map (lambda (x)
         (if (list? x)
             (square-tree2 x)
             (square x))) l))

(define (square-tree5 l)
  (map (lambda (x) ((if (list? x) square-tree5 square) x)) l))

(assert-many (lambda (f)
               (assert-equal '()    (f '()))
               (assert-equal '(4)   (f '(2)))
               (assert-equal '(4 9) (f '(2 3)))

               (assert-equal '(1 (4 (9 16) 25) (36 49))
                             (f '(1 (2 (3 4) 5) (6 7)))))
             square-tree1
             square-tree2
             square-tree3
             square-tree4
             square-tree5)
(done)
