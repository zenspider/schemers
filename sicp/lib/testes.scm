(module testes *

        (import scheme chicken test)

        (define false #f)
        (define true #t)
        (define null '())

        (define passed? #t)

        (define (failed!)
          (set! passed? #f))

        (define-syntax xassert
          (syntax-rules ()
            ((_ exp) (if exp (display ".")
                         (begin
                           (failed!)
                           (display "F: (xassert ")
                           (write (quote exp))
                           (display ")")
                           (newline)
                           (display ";; => ")
                           (display "(xassert ")
                           (write exp)
                           (display ")")
                           (newline))))))

        (define-syntax assert-equal
          (syntax-rules ()
            ((_ exp act) (if (equal? exp act)
                             (display ".")
                             (begin
                               (failed!)
                               (newline)
                               (display ";; F: (assert-equal ")
                               (write (quote exp))
                               (display " ")
                               (write (quote act))
                               (display ")")
                               (newline)
                               (display ";; => (assert-equal ")
                               (write exp)
                               (newline)
                               (display ";;                  ")
                               (write act)
                               (display ")")
                               (newline))))))

        (define (assert-float-d x y d)
          (xassert (< (abs (- x y)) d)))

        (define (assert-float x y)
          (assert-float-d x y 0.00001))

        (define (include? x l)
          (or (null? l) (equal? x (car l)) (include? x (cdr l))))

        (define-syntax assert-include
          (syntax-rules ()
            ((_ x l) (if (include? x l) (display ".")
                         (begin
                           (failed!)
                           (display "F: (assert-include ")
                           (write (quote x))
                           (display " ")
                           (write (quote l))
                           (display ")")
                           (newline)
                           (display ";; => ")
                           (display "(assert-include ")
                           (write x)
                           (display " ")
                           (write l)
                           (display ")")
                           (newline))))))

        (define (assert-many tests . futs)
          (for-each tests futs))

        (define-syntax refute
          (syntax-rules ()
            ((_ exp) (if (not exp) (display ".")
                         (begin
                           (failed!)
                           (display "F: (refute ")
                           (write (quote exp))
                           (display ")")
                           (newline)
                           (display ";; => ")
                           (display "(refute ")
                           (write exp)
                           (display ")")
                           (newline))))))

        (define-syntax refute-include
          (syntax-rules ()
            ((_ x l) (if (not (include? x l)) (display ".")
                         (begin
                           (failed!)
                           (display "F: (refute-include ")
                           (write (quote x))
                           (display " ")
                           (write (quote l))
                           (display ")")
                           (newline)
                           (display ";; => ")
                           (display "(refute-include ")
                           (write x)
                           (display " ")
                           (write l)
                           (display ")")
                           (newline))))))

        (define (done)
          (display "done!")
          (newline)
          (when (not passed?) (error "failed tests"))
          (newline)))
