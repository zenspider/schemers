;;; Chapter 16: Ready, Set, Bang!

(use test)
(use data-structures)

;; pg 127

(define find                            ; from pg 113
  (lambda (n Ns Rs)
    (letrec ((A (lambda (ns rs)
                  (cond ((null? ns) #f)
                        ((= (car ns) n) (car rs))
                        (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (cons (deep (sub1 m)) '()))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (letrec ((D (lambda (m)
                  (if (zero? m)
                      'pizza
                      (cons (D (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)))))))

(test '(((pizza))) (deepM 3))

(define deepM                           ; call deepM instead of D
  (let ((Rs '())
        (Ns '()))
    (letrec ((D (lambda (m)
                  (if (zero? m)
                      'pizza
                      (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
              (let ((result (D n)))
                (set! Rs (cons result Rs))
                (set! Ns (cons n Ns))
                result)))))))

(test '(((pizza))) (deepM 3))

(define deepM                           ; merge letrec into let
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
             (if (zero? m)
                 'pizza
                 (cons (deepM (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(test '(((pizza))) (deepM 3))

(define deepM                           ; unfactor D
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (cons (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(test '(((pizza))) (deepM 3))

;; pg 132

(define counter)
(define set-counter)

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

(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
        (consC (deep (sub1 m)) '()))))

(test '(((pizza))) (deep 3))
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

;; (supercounter deep)
;; (test 500500 (counter))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result))))))

(set-counter 0)
(deepM 5)
(test 5 (counter))
(deepM 7)
(test 7 (counter))
(deepM 1000)
(test 1000 (counter))

;; pg 139

(use miscmacros)

(define rember1*C
  (lambda (a l)
    (letrec ((R (lambda (l oh)
                  (cond
                   ((null? l) (oh 'no))
                   ((atom? (car l)) (if (eq? (car l) a)
                                        (cdr l)
                                        (consC (car l) (R (cdr l) oh))))
                   (else (let ((new-car (let/cc oh (R (car l) oh))))
                           (if (atom? new-car)
                               (consC (car l) (R (cdr l) oh)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(test '(a c) (rember1*C 'b '(a b c)))
