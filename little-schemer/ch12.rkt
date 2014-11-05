#!/usr/local/bin/csi -s

(use test)

(define (Y outer)                       ; taken from ch09.scm
  (define (call f)
    (define (apply x) ((f f) x))
    (outer apply))
  (call call))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             ((null? lat) '())
             ((eq? a (car lat)) (mr (cdr lat)))
             (else (cons (car lat) (mr (cdr lat))))))))
     lat)))

(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))))

(length '(1 2 3))

(define multirember
  (lambda (a lat)
    (letrec ((mr (lambda (lat)
                   (cond ((null? lat) '())
                         ((eq? a (car lat)) (mr (cdr lat)))
                         (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))

(multirember 2 '(1 2 3 2 4 2 5))

;;; 12th Commandment
;;
;; Use (letrec ...) to remove arguments that do not change for
;; recursive applications

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multirember-f
  (lambda (test?)
    (letrec ((m-f (lambda (a lat)
                    (cond
                     ((null? lat) '())
                     ((test? (car lat) a)  (m-f a (cdr lat)))
                     (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))

(define member?
  (lambda (a lat)
    ((letrec ((yes? (lambda (l)
                      (cond ((null? l) #f)
                            ((eq? (car l) a) #t)
                            (else (yes? (cdr l)))))))
       yes?)
     lat)))

(define member?
  (lambda (a lat)
    ((letrec ((yes? (lambda (l)
                      (cond ((null? l) #f)
                            ((eq? (car l) a) #t)
                            (else (yes? (cdr l)))))))
       (yes? lat)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1)     (union (cdr set1) set2))))))

(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set)
                  (cond ((null? set) set2)
                        ((member? (car set) set2) (U (cdr set)))
                        (else (cons (car set) (U (cdr set))))))))
      (U set1))))

;;; 13th Commandment
;;
;; Use (letrec ...) to hide and to protect functions

(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set)
                  (cond ((null? set) set2)
                        ((M? (car set) set2)  (U (cdr set)))
                        (else (cons (car set) (U (cdr set)))))))
             (M? (lambda (a lat)
                   (letrec ((N? (lambda (lat)
                                  (cond ((null? lat) #f)
                                        ((eq? (car lat) a) #t)
                                        (else (N? (cdr lat)))))))
                     (N? lat)))))
      (U set1))))

(define two-in-a-row?
  (letrec ((W (lambda (a lat)
                (cond ((null? lat) #f)
                      (else (or (eq? (car lat) a)
                                (W (car lat) (cdr lat))))))))
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (W (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (lambda (tup)
    (letrec ((S (lambda (sss tup)
                  (cond ((null? tup) '())
                        (else (cons    (+ sss (car tup))
                                    (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))
