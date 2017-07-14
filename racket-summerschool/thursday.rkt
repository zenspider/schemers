#lang racket

(require (for-syntax syntax/parse))

(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_) #'#t]
    [(_ a:expr) #'a]
    [(_ a:expr b:expr ...) #'(if a (conjunction b ...) #f)]))

(module+ test
  (require rackunit)
  (require rackunit/log)
)

(module+ test
  (check-equal? (conjunction) #t)
  (check-equal? (conjunction 1) 1)
  (check-equal? (conjunction 1 2) 2)
  (check-equal? (conjunction 1 2 3) 3))

;; http://www.ccs.neu.edu/home/matthias/summer-school/thu-mor.html

;; Exercise 21. Develop the compile-time function disjunction2
;;              (without using or). The function deals with binary
;;              disjunctions of the shape (disjunction2 lhs:expr
;;              rhs:expr)
;;
;; After testing, run the expression (disjunction2 (displayln 1)
;; (displayln 2)).

(define-syntax (dis stx)
  (syntax-parse stx
    [(_) #'#f]
    [(_ a:expr) #'(let ([r a]) (if r r #f))]
    [(_ a:expr b:expr ...) #'(if a #t (dis b ...))]
    ))

(module+ test
  (check-equal? (dis) #f)
  (check-equal? (dis #f) #f)
  (check-equal? (dis #f #f) #f)
  (check-equal? (dis #f #f #f) #f)

  (check-equal? (dis 1) 1)
  (check-equal? (dis #f 1) 1)
  (check-equal? (dis #f #f 1) 1))

;; Exercise 22. Develop the compile-time function block (without using
;;              let*). The function implements syntax of the shape
;;
;; (block ([x1:id rhs1:expr]
;;         [x2:id rhs2:expr]
;;         ...
;;         [x3:id rhs3:expr])
;;        body:expr)
;;
;; The identifier x1 is visible in rhs2, but not vice versa, and so
;; on. All x1 thru x3 are visible in body.
;;
;; The evaluation initializes x1 to rhs1, x2 to rhs2, and so on. Once
;; all identifiers are initialized, body is evaluated. image

(define-syntax (block stx)
  (syntax-parse stx                     ; converting to lambda is trivial
    [(_ () body:expr) #'body]
    [(_ ([id:id val:expr] [id2:id val2:expr] ...) body:expr)
     #'(let ([id val])
         (block ((id2 val2) ...) body))]))

(module+ test
  (check-equal? (block ([a 1]
                        [b 2]
                        [c (+ a b)])
                       c)
                3))

;; Exercise 23. Develop the compile-time function for-loop. It
;;              realizes the following syntax trees as bounded loops
;;              over integers:
;;
;; (for-loop ([i e0 limit]) body)
;; or
;; (for-loop ([i e0 limit stride]) body)

(define-syntax (for-loop stx)
  (syntax-parse stx
    [(_ (id:id start:expr stop:expr stride:expr) body:expr)
     #'(let ([start* start]
             [stop* stop])
         (let loop ([id start*])
           (let ([result body])
             (if (> (+ id stride) stop*)
                 result
                 (loop (+ id stride))))))]
    [(_ (id:id start:expr stop:expr) body:expr)
     #'(for-loop (id start stop 1) body)]))

(module+ test
  (check-equal? (for-loop [x 0 10  ] x) 10)
  (check-equal? (for-loop [x 0 10 3] x) 9))


;; Exercise 24. Develop the compile-time function while, which adds
;;              while-do loops to Racket. Create your favorite syntax.

(define-syntax (while stx)
  (syntax-parse stx
    [(_ test:expr body:expr ...)
     #'(let loop ()
         (when test
           body ...
           (loop)))]))

(module+ test
  (check-equal? (let ([x 0]) (while (< x 3) (set! x (add1 x))) x)
                3))


;; Exercise 25. Develop the compile-time function dispatch. It
;;              translates syntax trees of this shape...

(define-syntax (dispatch stx)
  (syntax-parse stx                     ; cheating...
    [(_ e:expr (k:id v:expr) ... (default e-def:expr))
     #'(case e
         [(k) v] ...
         [else e-def])]))

(module+ test

  (define (next-state-of-traffic-light current)
   (dispatch current
             [red        'yellow-red]
             [yellow-red 'green]
             [green      'yellow]
             [yellow     'red]
             [default
               (if (symbol? current)
                   'blinking-red
                   (error "really bad things happened"))]))

  (check-equal? (next-state-of-traffic-light 'red) 'yellow-red))

;; http://www.ccs.neu.edu/home/matthias/summer-school/thu-aft.html

;; Exercise 26. Develop a compile-time function that adds:
;;
;;                (defun (f:id x:id) e:expr)
;;
;;              to Racket where f becomes the identifier of a
;;              function, x its argument, and f the function body.

(define-syntax (defun stx)              ; too simple? did I misunderstand?
  (syntax-parse stx
    [(defun (f:id x:id) e:expr)
     #'(define (f x) e)]))

(module+ test
  (defun (id*id x) (* x x))

  (check-equal? (id*id 5) 25))

;; Exercise 27. Develop a compile-time function that implements a
;;              variant of like if that reacts only to #true and
;;              #false in the test position.

(define-syntax (iff stx)
  (syntax-parse stx
    [(_ c:boolean t:expr f:expr)
     (if c t f)]))

(module+ test
  (void (test-log #:display? #t #:exit? #f)))
