(module streams
  (export stream-add
          stream-car
          stream-cdr
          stream-cons
          stream-display
          stream-enumerate-interval
          stream-head
          stream-filter
          stream-for-each
          stream-map
          stream-null?
          stream-ref)

  (import scheme chicken extras)

  (define-syntax stream-cons
    (syntax-rules ()
      ((_ a b) (cons a (delay b)))))

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

  (define (stream-null? stream) (null? stream))

  (define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

  (define the-empty-stream '()))
