#!/usr/bin/env csi -s

(use test)
(require-library compiler)
(import compiler)

;;; Exercise 5.50

;; Use the compiler to compile the metacircular evaluator of section
;; *Note 4-1:: and run this program using the register-machine
;; simulator. (To compile more than one definition at a time, you can
;; package the definitions in a `begin'.) The resulting interpreter
;; will run very slowly because of the multiple levels of
;; interpretation, but getting all the details to work is an
;; instructive exercise.

(define eval-sexp #f)

(with-input-from-file "lib/eval.scm"
  (lambda ()
    (set! eval-sexp (drop (read) 5))))

(append-primitive-procedures
 '<           <
 'caar        caar
 'cadddr      cadddr
 'caddr       caddr
 'cadr        cadr
 'cddr        cddr
 'eof-object? eof-object?
 'error       error
 'list        list
 'not         not
 'number?     number?
 'pair?       pair?
 'set-car!    set-car!
 'set-cdr!    set-cdr!
 'string?     string?
 'symbol?     symbol?
 'zip         zip)

(define (test-sexp exp)
  (append
   '(begin)
   (append
    eval-sexp
    `((define my-env (setup-environment))
      (eval ,exp my-env)))))

(test-group "holy crap I'm done!"
  (assert-compile  42 (test-sexp '42))
  (assert-compile  24 (test-sexp '24))
  (assert-compile  #t (test-sexp 'true))
  (assert-compile  #f (test-sexp 'false))
  (assert-compile   3 (test-sexp '(+ 1 2)))
  (assert-compile 120 (test-sexp '(* 1 2 3 4 5)))
  (assert-compile "good" (test-sexp '(begin
                                       (define (append x y)
                                         (if (null? x)
                                             y
                                             (cons (car x) (append (cdr x) y))))
                                       (let ((l (append '(a b c) '(d e f))))
                                         (cond
                                          ((eq? (length l) 5) "bad")
                                          ((eq? (length l) 6) "good")
                                          (else 'else))))))

  ;; HACK; prolly should be '*undefined* or somesuch
  (assert-compile #f (test-sexp '(begin
                                   (define (append x y)
                                     (if (null? x)
                                         y
                                         (cons (car x) (append (cdr x) y))))
                                   (let ((l (append '(a b c) '(d))))
                                     (cond
                                      ((eq? (length l) 5) "bad")
                                      ((eq? (length l) 6) "good"))))))

  (assert-compile "else" (test-sexp '(begin
                                       (define (append x y)
                                         (if (null? x)
                                             y
                                             (cons (car x) (append (cdr x) y))))

                                       (let ((l (append '(a b c) '(d))))
                                         (cond
                                          ((eq? (length l) 5) "bad")
                                          ((eq? (length l) 6) "good")
                                          (else "else"))))))

  (assert-compile 120 (test-sexp '(begin
                                    (define (factorial-r n)
                                      (if (= n 1)
                                          1
                                          (* (factorial-r (- n 1)) n)))
                                    (factorial-r 5))))

  (assert-compile 120 (test-sexp '(begin
                                    (define (factorial-i n)
                                      (define (iter product counter)
                                        (if (> counter n)
                                            product
                                            (iter (* counter product)
                                                  (+ counter 1))))
                                      (iter 1 1))
                                    (factorial-i 5))))

  (assert-compile 55 (test-sexp '(begin
                                   (define (fib n)
                                     (if (< n 2)
                                         n
                                         (+ (fib (- n 1)) (fib (- n 2)))))
                                   (fib 10)))))
