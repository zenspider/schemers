#!/usr/bin/env csi -s

(require rackunit)
(require-library eval)
(import eval)

;;; Exercise 4.9

;; Many languages support a variety of iteration
;; constructs, such as `do', `for', `while', and `until'.  In Scheme,
;; iterative processes can be expressed in terms of ordinary
;; procedure calls, so special iteration constructs provide no
;; essential gain in computational power.  On the other hand, such
;; constructs are often convenient.  Design some iteration
;; constructs, give examples of their use, and show how to implement
;; them as derived expressions.

(define (for->for-each exp)
  (let ((var (cadr exp))
        (lst (cadddr exp))
        (bdy (cddddr exp)))
    (list 'for-each (make-lambda (list var) bdy) lst)))

(define tmp-count 0)
(define (new-tmp)
  (set! tmp-count (+ 1 tmp-count))
  (string->symbol (string-append "temp" (number->string tmp-count))))

(define (while->named-let exp)
  (let ((tst (cadr exp))
        (tmp (new-tmp))
        (body (cdddr exp)))
    (list 'let tmp '()
          (make-if (list tst)
                   (make-begin (append body (list (list tmp))))
                   #f))))

(test-group "4.09"
  (test '(lambda (a b c) (e1) (e2) (e3))
        (make-lambda '(a b c) '((e1) (e2) (e3))))

  (test '(for-each (lambda (i) body1 body2) list)
        (for->for-each '(for i in list body1 body2)))

  (test '(let temp1 ()
           (if (tst)
               (begin
                 body1
                 body2
                 (temp1))))
        (while->named-let '(while tst do body1 body2))))
