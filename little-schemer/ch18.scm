;;; Chapter 16: Ready, Set, Bang!

(use test)
(use data-structures)

(define (bons kar)
  (let ((kdr '()))
    (lambda (selector)
      (selector (lambda (x) (set! kdr x)) kar kdr))))

;; (define (kons kar kdr)
;;   (lambda (selector)
;;     (selector kar kdr)))

(define (kar c)
  (c (lambda (s a d) a)))

(define (kdr c)
  (c (lambda (s a d) d)))

(define (set-kdr c x)
  ((c (lambda (s a d) s)) x))

(define (kons a d)
  (let ((c (bons a)))
    (set-kdr c d)
    c))

(define k)
(define (set-kounter n)
  (set! k n))
(set-kounter 0)
(define (kounter) k)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lots m)
  (cond ((zero? m) '())
        (else (kons 'egg (lots (sub1 m))))))

(define (lenkth l)
  (cond ((null? l) 0)
        (else (add1 (lenkth (kdr l))))))

(define (add-at-end l)
  (cond ((null? (kdr l))
         (konsC (kar l) (kons 'egg '())))
        (else (konsC (kar l) (add-at-end (kdr l))))))

(define (add-at-end-too l)
  (letrec ((A (lambda (ls)
                (cond ((null? (kdr ls))
                       (set-kdr ls (kons 'egg '())))
                      (else (A (kdr ls)))))))
    (A l)
    l))

(test '(egg egg egg) (lots 3))
(test 3 (lenkth (lots 3)))

(set-kounter 0)
(test '(egg egg egg egg) (add-at-end (lots 3)))
(test 3 (kounter))

(set-kounter 0)
(test '(egg egg egg egg) (add-at-end-too (lots 3)))
(test 0 (kounter))
