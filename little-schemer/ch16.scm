;;; Chapter 16: Ready, Set, Bang!

(use test)
(use data-structures)

;;; 17th Commandment:
;;
;; Use (set! x ...) for (let ((x ...)) ...) only if there is at least
;; one (lambda ...) between it and the (let ...), or if the new value
;; for x is a function that refers to x.

;;; 19th Commandment:
;; Use (set! ...) to remember valuable things between two distinct
;; uses of a function.

(define (find n Ns Rs)
  (letrec ((A (lambda (ns rs)
                (cond ((= (car ns) n) (car rs))
                      (else (A (cdr ns) (cdr rs)))))))
    (A Ns Rs)))

(define (deep m)
  (cond ((zero? m) 'pizza)
        (else (cons (deepM (sub1 m)) '()))))

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (n)
      (if (member n Ns)
          (find n Ns Rs)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(test '(((pizza))) (deep 3))

(test '(((pizza))) (deepM 3))
;; (test '((((pizza)))) (identity Rs))
;; (test '(3) (identity Ns))

;; pg 119:

(define (length l)
  (cond
   ((null? l) 0)
   (else (add1 (length (cdr l))))))

(define length (lambda (l) 0))

(set! length
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
             ((null? l) 0)
             (else (add1 (h (cdr l)))))))
    h))

;; pg 122

(define L
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))

(define length
  (let ((h (lambda (l) 0)))
    (set! h (L (lambda (arg) (h arg))))
    h))

(test 3 (length '(1 2 3)))

(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define Y-bang
  (lambda (f)
    (letrec ((h (f lambda (arg) (h arg))))
      h)))

(define length (Y! L))

(test 3 (length '(1 2 3)))

(define D
  (lambda (depth*)
    (lambda (s)
      (cond ((null? s) 1)
            ((atom? (car s)) (depth* (cdr s)))
            (else (max (add1 (depth* (car s)))
                       (depth* (cdr s))))))))

(define depth* (Y! D))

(test 3 (depth* '(a (b (c)))))
