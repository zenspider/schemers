#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.41

;; Write a procedure `find-variable' that takes as arguments a
;; variable and a compile-time environment and returns the lexical
;; address of the variable with respect to that environment. For
;; example, in the program fragment that is shown above, the
;; compile-time environment during the compilation of expression <E1>
;; is `((y z) (a b c d e) (x y))'. `Find-variable' should produce
;;
;;      (find-variable 'c '((y z) (a b c d e) (x y)))
;;      (1 2)
;;
;;      (find-variable 'x '((y z) (a b c d e) (x y)))
;;      (2 0)
;;
;;      (find-variable 'w '((y z) (a b c d e) (x y)))
;;      not-found

;; Sucks because there is no way to get obj + index via a findy function
(define (find-variable var env)
  (let ((y (list-index (lambda (l) (member var l)) env)))
    (if y
        (list y (list-index (lambda (x) (eq? var x)) (list-ref env y)))
        'not-found)))

(let ((env '((y z)
             (a b c d e)
             (x y))))
  (test '(1 2)     (find-variable 'c env))
  (test '(2 0)     (find-variable 'x env))
  (test 'not-found (find-variable 'w env)))
