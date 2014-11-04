#lang racket

(require (only-in r5rs set-cdr!)) ; HACK

(define evaluate.tracing false)

(define (evaluate e env)
  (if (pair? e)
      (case (car e)
        [(quote)  (cadr e)]
        [(if)     (if (evaluate (cadr   e) env)
                      (evaluate (caddr  e) env)
                      (evaluate (cadddr e) env))]
        [(begin)  (eprogn (cdr e) env)]
        [(set!)   (update! (cadr e) env (evaluate (cddr e) env))]
        [(lambda) (make-function (cadr e) (cddr e) env)]
        [else     (let ([fn        (evaluate (car e) env)]
                        [arguments (evlis (cdr e) env)])
                    (when evaluate.tracing
                      (display `(calling ,(car e) with . ,arguments)
                               (current-error-port))
                      (newline))
                    (invoke fn arguments))])
      (cond [(symbol? e) (lookup e env)]
            [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e]
            [else (wrong "Cannot evaluate" e)])))

(define wrong error)                    ; hack?

(define (eprogn exps env)               ; 1.4.3
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))

(define empty-begin 813)                ; no clue what this is a reference to

(define (evlis exps env)                ; 1.4.6 + Exercise 1.2
  (define (helper exps)
    (cons (evaluate (car exps) env)
          (if (pair? (cdr exps))
              (helper (cdr exps))
              empty)))
  (if (pair? exps)
      (helper exps)
      empty))

(define env.init empty)                 ; 1.5 TODO: remove?

(define (lookup id env)                 ; 1.5
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))

(define (update! id env value)          ; 1.5
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 (value))
          (update! id (cdr env) value))
      (wrong "No such binding" id)))

(define (make-function variables body env) ; 1.6
  (lambda (values)
    (eprogn body (extend env variables values))))

(define (extend env variables values)   ; 1.5 -- I don't like this impl
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (wrong "Too few values")))
        ((null? variables)
         (if (null? values)
             env
             (wrong "Too many values")))
        ((symbol? variables)            ; wtf
         (cons (cons variables values) env))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

(define env.global env.init)

(define-syntax definitial
  (syntax-rules ()
    ((_ name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))))
    ((_ name value)
     (begin (set! env.global (cons (cons 'name value) env.global))))))

(define-syntax defprimitive
  (syntax-rules ()
    ((_ name value arity)
     (definitial name
       (lambda (values)
         (if (= arity (length values))
             (apply value values)       ; the real apply of scheme
             (wrong "Incorrect arity" (list name values))))))))

(definitial t #t)
(definitial f #f)
(definitial nil empty)

(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)
(definitial trace-on  (lambda (args) (set! evaluate.tracing true)))
(definitial trace-off (lambda (args) (set! evaluate.tracing false)))

(defprimitive cons     cons     2)
(defprimitive car      car      1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive +        +        2)
(defprimitive eq?      eq?      2)
(defprimitive <        <        2)

(define (chapter1-scheme)
  (let toplevel ()
    (display "> ")
    (display (evaluate (read) env.global))
    (newline)
    (toplevel)))

(module+ main
  (chapter1-scheme))

(module+ test
  (require rackunit)

  (check-equal? (list 2 3)
                (let ((a 1))
                  ((let ((a 2)) (lambda (b) (list a b)))
                   3))))

#;(check-equal? (list 2 3)
(evaluate '(((lambda (a) (lambda (b) (list a b))) 1) 2)
          env.init))