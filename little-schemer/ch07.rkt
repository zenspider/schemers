#lang racket/base

(provide build first second a-pair? revpair)

(require rackunit)
(require "lib/shared.rkt")
(require "ch02.rkt")                    ; member?
(require "ch03.rkt")                    ; multirember

;;; Chapter 7
;; pg 111

(define set?
  (lambda (lat)
    (cond
     [(null? lat) #t]
     [(member? (car lat) (cdr lat)) #f]
     [else (set? (cdr lat))])))

(check-equal? (set? '(a b a c))
              #f)
(check-equal? (set? '(a b c d))
              #t)
(check-equal? (set? '())
              #t)
(check-equal? (set? '(apple 3 pear 4 9 apple 3 4))
              #f)
(check-equal? (set? '(apple 3 pear 4 9))
              #t)

;; pg 112

(define makeset1
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [(member? (car lat) (cdr lat)) (makeset1 (cdr lat))]
     [else (cons (car lat)
                 (makeset1 (cdr lat)))])))

(check-equal? (makeset1 '(a b c b d a e b))
              '(c d a e b))

(define makeset
  (lambda (lat)
    (cond
     [(null? lat) '()]
     [else
      (cons (car lat)
            (makeset (multirember (car lat) (cdr lat))))])))

(check-equal? (makeset '(a b c b d a e b))
              '(a b c d e))

;; pg 113

(check-equal? (makeset '(a 3 p 4 9 a 3 4))
              '(a 3 p 4 9))

(define subset1?
  (lambda (set1 set2)
    (cond
     [(null? set1) #t]
     [(member? (car set1) set2)
      (subset1? (cdr set1) set2)]
     [else #f])))

(check-equal? (subset1? '(5 c w) '(5 h 2 p f c a l d w))
              #t)
(check-equal? (subset1? '(4 p o h) '(4 p c a 5 oz h))
              #f)

;; pg 114

(define subset?
  (lambda (set1 set2)
    [cond
     [(null? set1) #t]
     [else
      (and (member? (car set1) set2)
           (subset? (cdr set1) set2))]]))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(check-equal? (eqset? '(6 l c wi w) '(6 c wi l w))
              #t)

;; pg 115

(define intersect1?
  (lambda (set1 set2)
    (cond
     [(or (null? set1) (null? set2)) #f]
     [else
      (or (member? (car set1) set2)
          (intersect1? (cdr set1) set2))])))

;; I don't know how to write this... does scheme even have specal forms like this?
;; (define nor
;;   (lambda (&rest args)
;;     (and (not (car args))
;;          (nor (cdr args)))))
;; (nor #t #f)

(define intersect?
  (lambda (set1 set2)
    (and
     (not (null? set1))
     (not (null? set2))
     (or (member? (car set1) set2)
         (intersect? (cdr set1) set2)))))

(check-equal? (intersect? '(a b c d) '(d c e))
              #t)
(check-equal? (intersect? '(a b c) '(d e f))
              #f)
(check-equal? (intersect? '() '(d c e))
              #f)
(check-equal? (intersect? '(d c e) '())
              #f)

;; pg 116

(define intersect
  (lambda (set1 set2)
    (cond [(null? set1) '()]
          [(member? (car set1) set2)
           (cons (car set1)
                 (intersect (cdr set1) set2))]
          [else (intersect (cdr set1) set2)])))

(check-equal? (intersect '(stewed tomatoes and macaroni casserole)
                         '(macaroni and cheese))
              '(and macaroni))

(define union
  (lambda (set1 set2)
    (cond [(null? set1) set2]
          [(member? (car set1) set2)
           (union (cdr set1) set2)]
          [else (cons (car set1)
                      (union (cdr set1) set2))])))

(check-equal? (union '(stewed tomatoes and macaroni casserole)
                     '(macaroni and cheese))
              '(stewed tomatoes casserole macaroni and cheese))

;; pg 117

(define difference
  (lambda (set1 set2)
    (cond
     [(null? set1) '()]
     [(member? (car set1) set2)
      (difference (cdr set1) set2)]
     [else (cons (car set1)
                 (difference (cdr set1) set2))])))

(check-equal? (difference '(a b c d e) '(a c e))
              '(b d))

(define intersectall
  (lambda (l-set)
    (cond [(null? (cdr l-set)) (car l-set)]
          [else (intersect (car l-set)
                           (intersectall (cdr l-set)))])))

(check-equal? (intersectall '((a b c) (c a d e) (e f g h a b)))
              '(a))
(check-equal? (intersectall '((6 pears and)
                              (3 peaches and 6 peppers)
                              (8 pears and 6 plums)
                              (and 6 prunes with some apples)))
              '(6 and))

;; pg 118

(define a-pair?
  (lambda (x)
    (cond [(atom? x) #f]
          [(null? x) #f]
          [(null? (cdr x)) #f]
          [(null? (cdr (cdr x))) #t]
          [else #f])))

(check-equal? (a-pair? '(full (house)))
              #t)

;; pg 119

(define first car)
(define second cadr)
(define build (lambda (s1 s2) (cons s1 (cons s2 null))))
(define third caddr)

;; pg 120

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(check-equal? (fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
              #t)

(define revrel1
  (lambda (rel)
    (cond
     [(null? rel) '()]
     [else (cons (build (second (car rel))
                        (first  (car rel)))
                 (revrel1 (cdr rel)))])))

(check-equal? (revrel1 '((8 3) (4 2) (7 6) (6 2) (3 4)))
              '((3 8) (2 4) (6 7) (2 6) (4 3)))

;; pg 121

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
     [(null? rel) '()]
     [else (cons (revpair (car rel))
                 (revrel (cdr rel)))])))

(check-equal? (revrel '((8 3) (4 2) (7 6) (6 2) (3 4)))
              '((3 8) (2 4) (6 7) (2 6) (4 3)))

;; pg 122

(define seconds
  (lambda (l)
    (cond [(null? l) '()]
          [else (cons (cadr l)
                      (seconds (cdr l)))])))

(define fullfun?
  (lambda (fun)
    (and (fun? fun)
         (set? (revrel fun)))))

(check-equal? (fullfun? '((grape raisin) (plum prune) (stewed grape)))
              #t)
