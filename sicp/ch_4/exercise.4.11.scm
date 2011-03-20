#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.11

;; Instead of representing a frame as a pair of
;; lists, we can represent a frame as a list of bindings, where each
;; binding is a name-value pair.  Rewrite the environment operations
;; to use this alternative representation.

(define (make-frame vars vals)
  (zip vars vals))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (car frame) (cdr frame)))
  (set-car! frame (list var val)))

;; This is fucking stupid. This is the result of having a poor data
;; structure to begin with coupled with a poorly thought out
;; assignment order. Apparently I just did 4.12 as well as 4.11
;; because it didn't make sense to force a broken design on a proper
;; data structure. Oh look... I can use assoc now like a real person.

(define (find-pair-in-frame frame var)
  (cond ((null? frame)          null)
        ((eq? var (caar frame)) (car frame))
        (else (find-pair-in-frame (cdr frame) var))))

(define (find-pair-in-env env var)
  (if (eq? env the-empty-environment) null
      (let ((pair (find-pair-in-frame (first-frame env) var)))
        (if (null? pair) (find-pair-in-env (rest-frames env) var)
            pair))))

(define (lookup-variable-value var env)
  (let ((pair (find-pair-in-env env var)))
    (if (null? pair)
        (error "Unbound variable" var)
        (cadr pair))))

(define (set-variable-value! var val env)
  (let ((pair (find-pair-in-env env var)))
    (if (null? pair)
        (error "Unbound variable -- SET!" var)
        (set-cdr! pair (list val)))))

(define (define-variable! var val env)
  (let ((pair (find-pair-in-frame (first-frame env) var)))
    (if (null? pair)
        (add-binding-to-frame! var val (first-frame env))
        (set-cdr! pair (list val)))))

(test-group "4.11"
    (define (identity x) x) ; test REALLY doesn't like testing env

    (let ((env (extend-environment '(a b) '(42 24) the-empty-environment)))

      (test `(proc (a b c) 42 ,env) (make-procedure '(a b c) 42 env))

      (test '((a 42) (b 24))   (make-frame '(a b) '(42 24)))
      (test '(((a 42) (b 24))) (identity env))
      (test '((a 42) (b 24))   (first-frame env))

      (test '(((a 42) (b 24))) (identity env))
      (add-binding-to-frame! 'c 314 (first-frame env))
      (test '(((c 314) (a 42) (b 24))) (identity env))

      (test '(a 42) (find-pair-in-frame (first-frame env) 'a))
      (test null    (find-pair-in-frame (first-frame env) 'z))

      (test '(a 42) (find-pair-in-env env 'a))
      (test null    (find-pair-in-env env 'z))

      (test null     (find-pair-in-env '(((a 42) (b 24)) ((c 314))) 'z))
      (test '(b 24)  (find-pair-in-env '(((a 42) (b 24)) ((c 314))) 'b))
      (test '(c 314) (find-pair-in-env '(((a 42) (b 24)) ((c 314))) 'c))

      (define-variable! 'c 42 env)
      (define-variable! 'd 24 env)

      (test 42 (lookup-variable-value 'c env))

      (test 42    (lookup-variable-value 'c env))
      (test 24    (lookup-variable-value 'd env))
      (set-variable-value! 'c 'woot env)
      (set-variable-value! 'd 'boo! env)
      (test 'woot (lookup-variable-value 'c env))
      (test 'boo! (lookup-variable-value 'd env))))