#!/usr/bin/env csi -s

(use test)
(require-library machine)
(import machine)
(require-library ec-eval)
(import ec-eval)

;;; Exercise 5.23

;; Extend the evaluator to handle derived expressions such as `cond',
;; `let', and so on (section *Note 4-1-2::). You may "cheat" and
;; assume that the syntax transformers such as `cond->if' are
;; available as machine operations.(1)

(define input-src
  '((exp (begin
           (define (append x y)
             (if (null? x)
                 y
                 (cons (car x) (append (cdr x) y))))

           (let ((l (append '(a b c) '(d e f))))
             (cond
              ((eq? (length l) 6) 'woot)
              (else 'boo)))))))

(assert-machine ec-eval input-src 'val 'woot)
