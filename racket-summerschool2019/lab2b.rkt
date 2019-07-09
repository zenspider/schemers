#lang racket

(require syntax/parse/define racket/stxparam
         (for-syntax racket/syntax))

;; Exercise 16. Use let/ec to make a version of define that defines a
;; return statement.

(call/ec (lambda (return)
          (displayln "woot")
          (return 42)
          (displayln "NOPE")))

(define-simple-macro (define/ec? (name:id arg:id ...) e:expr ...)
  #:with our-return (datum->syntax #'name 'return)
  (define (name arg ...)
    (call/ec (lambda (our-return)
               e ...))))

(define-syntax-parameter return #f)
(define-simple-macro (define/ec (name:id arg:id ...) e:expr ...)
  #:with our-return (datum->syntax #'name 'return)
  (define (name arg ...)
    (syntax-parameterize ([return (make-rename-transformer #'our-return)])
      (call/ec (lambda (our-return)
                 e ...)))))

(define/ec? (thingy? x)
  (printf "woot ~a~n" x)
  (return 42)
  (displayln "NOPE"))

(thingy? 'yay)

(define/ec (thingy x)
  (printf "woot ~a~n" x)
  (return 42)
  (displayln "NOPE"))

(thingy 'yay)

;; Exercise 17. Use let/ec to make a while macro that supports break
;; and continue.

#;
(while #t
  (displayln "woot!")
  (break))

;; Exercise 18. Write a version of if that provides access to the
;; value of the condition via it.

(define-syntax-parameter it #f)
(define-simple-macro (if! c:expr t:expr f:expr)
    (let ([our-it c])
      (syntax-parameterize ([it (make-rename-transformer #'our-it)])
        (if our-it t f)))
)
(if! 'hello
     (displayln it)
     #f)

;; Exercise 19. Write a version of cond that uses the if from the
;; previous exercise to also provide access the value of the question.

#| fuck this...

(define-simple-macro (cond! clause rest ...)
  (if! clause (cond! rest ...)))

(cond! ('hello (displayln it))
       (t #f))
|#

;; Exercise 20. Write a macro that allows you define bindings related
;; to other bindings. For example, a user could write:
;;
;; (struct posn (x y))
;; (define-related (define-posn p)
;;  [x (posn-x p)]
;;  [y (posn-y p)])
;;
;; (define (magnitude p)
;;  (define-posn p)
;;  (+ x y))
;;
;; (magnitude (posn 3 4))
;; ; => 7

(define-simple-macro (define-related (name:id arg:id)
                       [var:id val:expr] ...+)
  (define-simple-macro (name arg)
    ;; #:with arg (datum->syntax arg 'arg)
    (begin
      (define arg )
      (define var val) ...)))

(struct posn (x y))

(define-related (define-posn p)
  [x (posn-x p)]
  [y (posn-y p)])

#;
(define (magnitude p)
  (define-posn p)
  (+ x y))

#;
(magnitude (posn 3 4))


;; Exercise 21. Modify the classy macro to add default values to the
;; fields.

;; Exercise 22. Modify the classy macro to allow any number of fields
;; to be updated with update.
