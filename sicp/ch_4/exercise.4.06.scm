#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.6

;; `Let' expressions are derived expressions, because
;;
;;      (let ((<VAR_1> <EXP_1>) ... (<VAR_N> <EXP_N>))
;;        <BODY>)
;;
;; is equivalent to
;;
;;      ((lambda (<VAR_1> ... <VAR_N>)
;;         <BODY>)
;;       <EXP_1>
;;       ...
;;       <EXP_N>)
;;
;; Implement a syntactic transformation `let->combination' that
;; reduces evaluating `let' expressions to evaluating combinations of
;; the type shown above, and add the appropriate clause to `eval' to
;; handle `let' expressions.

(define (let-args exp)
  (map car (cadr exp)))

(define (let-vals exp)
  (map cadr (cadr exp)))

(define let-body cddr)

(define (let->call args vals body)
  (append (list (append (list 'lambda args) body)) vals))

(define (let->combination exp)
  (let->call (let-args exp) (let-vals exp) (let-body exp)))

(test-group "4.06"
            (define mylet '(let ((a 1)
                                 (b 2)
                                 (c 3))
                             body))

                        (test '(a b c) (let-args mylet))
                        (test '(1 2 3) (let-vals mylet))
                        (test '(body)  (let-body mylet))

            (test '((lambda (a b c) body) 1 2 3)
                  (let->combination mylet)))


