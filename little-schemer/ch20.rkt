;;; Chapter 16: Ready, Set, Bang!

#lang racket/base

(module+ test
  (require rackunit))

(require "lib/shared.rkt")

;;; Miscellany

(define (puts name . vals)
  (when #f
    (printf "~s~n" (cons name vals))))

(define abort #f)                          ; for call/cc

(define global-table #f)

(define (a-prim p)
  (lambda (args-in-a-list) (p (car args-in-a-list))))

(define (b-prim p)
  (lambda (args-in-a-list) (p (car args-in-a-list)
                              (car (cdr args-in-a-list)))))

(define (beglis es table)
  (cond ((null? (cdr es)) (meaning (car es) table))
        (else ((lambda (val) (beglis (cdr es) table))
               (meaning (car es) table)))))

(define (define? e)
  (cond ((atom? e) #f)
        ((atom? (car e)) (eq? (car e) 'define))
        (else #f)))

(define (evcon lines table)
  (puts 'evcon lines)
  (cond ((else? (question-of (car lines)))
         (meaning (answer-of (car lines))  table))
        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table))
        (else (evcon (cdr lines) table))))

(define (evlis args table)
  (puts 'evlis args)
  (cond ((null? args) '())
        (else ((lambda (val) (cons val (evlis (cdr args) table)))
               (meaning (car args) table)))))

(define (extend name1 value table)
  (puts 'extend name1)
  (lambda (name2)
    (cond ((eq? name2 name1) value)
          (else (table name2)))))

(define (lookup table name)
  (puts 'lookup name)
  (table name))

(define (lookup-in-global-table name)
  (lookup global-table name))

(define (meaning e table)
  (puts 'meaning e)
  ((expression-to-action e) e table))

(define (multi-extend names values table)
  (cond ((null? names) table)
        (else (extend (car names) (car values)
                      (multi-extend (cdr names) (cdr values) table)))))

(define (the-empty-table)
  (lambda (name)
    (abort (cons 'no-answer (cons name '())))))

(define (the-meaning e)
  (meaning e lookup-in-global-table))

(define (value e)
  (puts 'value e)
  (let/cc the-end
          (set! abort the-end)
          (cond ((define? e) (*define e))
                (else (the-meaning e)))))

;;; Boxing:

(define (box it)
  (puts 'box it)
  (lambda (sel)
    (sel it (lambda (new) (set! it new)))))

(define (box-all vals)
  (cond ((null? vals) '())
        (else (cons (box (car vals))
                    (box-all (cdr vals))))))

(define (setbox box new)
  (box (lambda (it set) (set new))))

(define (unbox box)
  (puts 'unbox box)
  (box (lambda (it set) it)))

;;; Actions:

(define (*application e table)
  (puts 'application e)
  ((meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define (*cond e table)
  (puts 'cond e)
  (evcon (cond-lines-of e) table))

(define (*letcc e table)
  (let/cc skip
          (beglis (ccbody-of e)
                  (extend (name-of e)
                          (box (a-prim skip))
                          ))))

(define *const
  (let ((:cons    (b-prim cons))
        (:car     (a-prim car))
        (:cdr     (a-prim cdr))
        (:eq?     (b-prim eq?))
        (:atom?   (a-prim atom?))
        (:null?   (a-prim null?))
        (:zero?   (a-prim zero?))
        (:add1    (a-prim add1))
        (:sub1    (a-prim sub1))
        (:number? (a-prim number?)))
    (lambda (e table)
      (puts 'const e)
      (if (number? e)  e
          (case e
            ((#t)      #t)
            ((#f)      #f)
            ((cons)    :cons)
            ((car)     :car)
            ((cdr)     :cdr)
            ((eq?)     :eq?)
            ((atom?)   :atom?)
            ((null?)   :null?)
            ((zero?)   :zero?)
            ((add1)    :add1)
            ((sub1)    :sub1)
            ((number?) :number?)
            (else      'fucked))))))

(define (*define e)
  (puts '*define e)
  (set! global-table (extend (name-of e)
                             (box (the-meaning (right-side-of e)))
                             global-table)))

(define (*identifier e table)
  (puts '*identifier e)
  (unbox (lookup table e)))

(define (*lambda e table)
  (puts '*lambda e)
  (lambda (args)
    (beglis (body-of e)
            (multi-extend (formals-of e)
                          (box-all args)
                          table))))

(define (*quote e table)
  (puts '*quote e)
  (text-of e))

(define (*set e table)
  (puts '*set e)
  (setbox (lookup table (name-of e))
          (meaning (right-side-of e) table)))

;;; Action Dispatch

(define (atom-to-action e)
  (puts 'atom-to-action e)
  (if (number? e)  *const
      (case e
        ((#t)      *const)
        ((#f)      *const)
        ((cons)    *const)
        ((car)     *const)
        ((cdr)     *const)
        ((null?)   *const)
        ((eq?)     *const)
        ((atom?)   *const)
        ((zero?)   *const)
        ((add1)    *const)
        ((sub1)    *const)
        ((number?) *const)
        (else      *identifier))))

(define (expression-to-action e)
  (puts 'expression-to-action e)
  (cond ((atom? e) (atom-to-action e))
        (else (list-to-action e))))

(define (list-to-action e)
  (puts 'list-to-action e)
  (cond ((atom? (car e))
         (case (car e)
           ((quote)  *quote)
           ((lambda) *lambda)
           ((letcc)  *letcc)
           ((set!)   *set)
           ((cond)   *cond)
           (else     *application)))
        (else        *application)))

;;; Stupid accessors

(define answer-of     cadr)
(define arguments-of  cdr)
(define body-of       cddr)
(define ccbody-of     cddr)
(define cond-lines-of cdr)
(define formals-of    cadr)
(define function-of   car)
(define name-of       cadr)
(define question-of   car)
(define text-of       cadr)

(define (right-side-of x)
  (cond ((null? (cddr x)) 0)
        (else (caddr x))))

(define (else? x)
  (cond ((atom? x) (eq? x 'else))
        (else #f)))

;;; Internal Functions:

(define (:car args-in-a-list)
  (car (car args-in-a-list)))

;;; Tests:

(define (I x) x) ; just for testing

(module+ test
  (check-equal? (value 3)
                3)
  (check-equal? (value '(cond (else 0)))
                0)
  (check-equal? (value '(cond ((null? (cons 0 '())) 0) (else 1)))
                1)

  (check-equal? (value '(define odd?
                          (lambda (n)
                            (cond ((zero? n) #f)
                                  (else (even? (sub1 n)))))))
                (void))

  (check-equal? (value '(define even?
                          (lambda (n)
                            (cond ((zero? n) #t)
                                  (else (odd? (sub1 n)))))))
                (void))


  (check-false (value '(odd? 2)))
  (check-true (value '(odd? 1))))
