;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(require "../sicp/lib/test.rkt")
(module+ test (require rackunit))

;; pg 127

(define find                            ; from pg 113
  (lambda (n Ns Rs)
    (letrec ((A (lambda (ns rs)
                  (cond ((null? ns) #f)
                        ((= (car ns) n) (car rs))
                        (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deep1
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep1 (sub1 m)) '()))))

(define deepM1
  (let ((Rs '())
        (Ns '()))
    (letrec ((D (lambda (m)
                  (if (zero? m)
                      'pizza
                      (cons (D (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (when (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)))))))

(test '(((pizza))) (deepM1 3))

(define deepM2                           ; call deepM2 instead of D
  (let ((Rs '())
        (Ns '()))
    (letrec ((D (lambda (m)
                  (if (zero? m)
                      'pizza
                      (cons (deepM2 (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (when (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)))))))

(test '(((pizza))) (deepM2 3))

(define deepM3                           ; merge letrec into let
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM3 (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (when (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(test '(((pizza))) (deepM3 3))

(define deepM4                           ; unfactor D
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (when (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (cons (deepM4 (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(test '(((pizza))) (deepM4 3))

;; pg 132

(define counter #f)
(define set-counter #f)

(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (n) (set! N n)))
    (lambda (x y)
            (set! N (add1 N))
            (cons x y))))

(test 0 (counter))
(test '(a) (consC 'a '()))
(test 1 (counter))

(define deep2
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep2 (sub1 m)) '()))))

(test '(((pizza))) (deep2 3))
(test 4 (counter))

(set-counter 0)
(test 0 (counter))

;; pg 134

(define supercounter
  (lambda (f)
    (letrec ((S (lambda (n)
                  (if (zero? n)
                      (f n)
                      (let ()
                        (f n)
                        (S (sub1 n)))))))
      (S 1000))))

;; (supercounter deep2)
;; (test 500500 (counter))

(define deepM5
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (when (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM5 (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(set-counter 0)
(test '(((((pizza))))) (deepM5 5))
(test 5 (counter))
(test `((,(void))) (deepM5 7))
(test 7 (counter))
(define _ignore_ (deepM5 1000))
(test 1000 (counter))

;; pg 139

(define rember1*C
  (lambda (a l)
    (letrec ((R (lambda (l oh)
                  (cond
                   ((null? l) (oh 'no))
                   ((atom? (car l)) (if (eq? (car l) a)
                                        (cdr l)
                                        (consC (car l) (R (cdr l) oh))))
                   (else (let ((new-car (let/cc oh (R (car l) oh))))
                           (when (atom? new-car)
                               (consC (car l) (R (cdr l) oh)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(test '(a c) (rember1*C 'b '(a b c)))
