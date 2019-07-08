#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define
         racket/list)

;; FIX: I lost the syntax, stx is undefined atm
;; (define-syntax forty-three/0
;;   (λ (stx)
;;     (syntax (car (cdr stx)))))

(define-syntax forty-three/1
  (λ (stx)
    (syntax-parse stx
      #;[ pattern answner ]
      [(_ x _ ...)
       (syntax x)]
      )))

(define-simple-macro (forty-three/2 x _ ...) x)

;; (+ (forty-three/0 1)
;;    (forty-three/0 3 2 1))

(+ (forty-three/1 1)
   (forty-three/1 3 2 1))

(+ (forty-three/2 1)
   (forty-three/2 3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial n)
  (if (zero? n) 1
      (* n (factorial (sub1 n)))))

(define (fibonacci n)
  (if (n . <= . 1) 1
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (bad-time-it computation)
  (define t0 (current-inexact-milliseconds))
  (begin0 (computation)
    (displayln (- (current-inexact-milliseconds) t0))))

(define-simple-macro (timeit expr)
  (begin
    (define t0 (current-inexact-milliseconds))
    (begin0 expr
      (displayln (- (current-inexact-milliseconds) t0)))))

(define before (current-inexact-milliseconds))
(factorial (fibonacci 10))
(define after (current-inexact-milliseconds))
(displayln (- after before))

(bad-time-it (lambda () (factorial (fibonacci 10))))

(printf "hrm~n")
(timeit
 (factorial (fibonacci 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map (λ (e) (string-append "Burnt " e))
     (list "Rickard" "Brandon"))

(define-simple-macro (simple-for/list ([name seq]) expr)
  (map (λ (name) expr) seq))

(simple-for/list ([e (list "Rickard" "Brandon")])
                 (string-append "Burnt " e))

(define-simple-macro (simple-for2/list ([name seq] ...) expr)
  (map (λ (name ...) expr) seq ...))


(simple-for2/list ([e (list "Rickard" "Brandon")]
                   [i (list 1 2)])
                  (format "~a. Burnt ~a" i e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (tree-match stx)
  (syntax-parse stx
    [(_ tree-var)
     #'(error "blah ~e" tree-var)]

    [(_ tree-var [(#:null) null-ans] more ...)
     #'(if (null? tree-var) null-ans
           (tree-match tree-var more ...))]

    [(_ tree-var [(#:number v) num-ans] more ...)
     #'(if (number? tree-var)
           (let ([v tree-var])
             num-ans)
           (tree-match tree-var more ...))]

    [(_ tree-var [(#:cons lhs rhs) cons-ans] more ...)
     #'(if (cons? tree-var)
           (let ([lhs (car tree-var)]
                 [rhs (cdr tree-var)])
             cons-ans)
           (tree-match tree-var more ...))]))

(define (sum-tree tree-var)
  (tree-match tree-var
              [(#:null) 0]
              [(#:number n) n]
              [(#:cons l r)
               (+ (sum-tree l)
                  (sum-tree r))]))

(define (count-tree tree-var)
  (tree-match tree-var
              [(#:null) 0]
              [(#:number n) 1]
              [(#:cons l r)
               (+ (count-tree l)
                  (count-tree r))]))

(count-tree '(1 2 (3 4) (5 6) 7))
(sum-tree '(1 2 (3 4) (5 6) 7))
