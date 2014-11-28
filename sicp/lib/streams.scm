#lang r5rs

(#%provide integers
           list->stream
           ones
           stream->list
           stream-add
           stream-car
           stream-cdr
           stream-cons
           stream-display
           stream-enumerate-interval
           stream-filter
           stream-for-each
           stream-head
           stream-map
           stream-null?
           stream-ref
           stream-scale
           the-empty-stream)

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons a b) (cons a (delay b)))))

(define ones     (stream-cons 1 ones))

(define integers (stream-cons 1 (stream-add ones integers)))

(define (stream-add s1 s2)
  (stream-map + s1 s2))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-display stream)
  (define (display-line x)
    (newline)
    (display x))
  (stream-for-each display-line stream))

(define (stream-enumerate-interval low high)
  (letrec ((next (lambda (n)
                   (if (> n high) the-empty-stream
                       (cons n (delay (next (+ n 1))))))))
    (next low)))

(define (stream-head s n)
  (if (= n 0) '()
      (cons (stream-car s) (stream-head (stream-cdr s) (- n 1)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s) 'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; (define (stream-map proc s)
;;   (if (stream-null? s) the-empty-stream
;;       (stream-cons (proc s)
;;                    (stream-map proc (stream-cdr s)))))

(define (stream-null? stream) (null? stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-scale s n)
  (stream-map (lambda (x) (* x n)) s))

(define (list->stream l)
  (if (null? l) the-empty-stream
      (stream-cons (car l) (list->stream (cdr l)))))

(define (stream->list stream)
  (if (stream-null? stream)
      '()
      (cons (stream-car stream)
            (stream->list (stream-cdr stream)))))


(define the-empty-stream '())
