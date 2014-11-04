#lang racket

(define (fetch   r)   (r 'fetch))
(define (assign  r v) ((r 'assign) v))
(define (save    r)   (r 'save))
(define (restore r)   (r 'restore))

(define (store r v)   ((r 'store) v))
(define (read r)      (r 'read))

(define (register)
  (let ((v null))
    (lambda (op)
      (case op
        ((read) v)
        ((store) (lambda (nv) (set! v nv)))))))

(define (push s v) ((s 'push) v))
(define (pop s) (s 'pop))

(define (stack)
  (let ((s empty))
    (lambda (op)
      (case op
        ((push) (lambda (top) (set! s (cons top s))))
        ((pop)  (if (not (null? s))
                    (let ((v (car s)))
                      (set! s (cdr s))
                      v)
                    (error "Stack ran out - POP")))))))

(define (standard-stack)
  (let ((r (register))
        (s (stack)))
    (lambda (op)
      (case op
        ((fetch)   (read r))
        ((assign)  (lambda (nv) (store r nv)))
        ((save)    (push s (read r)))
        ((restore) (store r (pop s)))))))

(define (push-optimizer)
  (let ((r (register))
        (s (stack))
        (state (register)))
    (store state 'available)
    (lambda (op)
      (case op
        ((fetch)   (read r))
        ((assign)  (lambda (nv)
                     (case (read state)
                       ((in-use)    (begin ; for indentation only
                                      (push s (read r))
                                      (store state 'available)
                                      (store r nv)))
                       ((available) (store r nv)))))
        ((save)    (case (read state)
                     ((in-use)    (push s (read r)))
                     ((available) (store r (pop s)))))

        ((restore) (case (read state)
                     ((in-use)    (store state 'available))
                     ((available) (store r (pop s)))))))))

(define (push-and-pop-optimizer)
  (let ((r (register))
        (s (stack))
        (state (register)))
    (store state 'available)
    (lambda (op)
      (case op
        ((fetch)   (case (read state)
                     ((in-use)    (read r))
                     ((available) (read r))
                     ((on-stack)  (begin
                                    (store r (pop s))
                                    (store state 'available)
                                    (read r)))))
        ((assign)  (lambda (nv)
                     (case (read state)
                       ((in-use)    (begin ; for indentation only
                                      (push s (read r))
                                      (store state 'available)
                                      (store r nv)))
                       ((available) (store r nv))
                       ((on-stack   (begin
                                      (pop s)
                                      (store state 'available)
                                      (store r nv)))))))
        ((save)    (case (read state)
                     ((in-use)    (push s (read r)))
                     ((available) (store state 'in-use))
                     ((on-stack)  (store state 'available))))

        ((restore) (case (read state)
                     ((in-use)    (store state 'available))
                     ((available) (store state 'on-stack))
                     ((on-stack)  (pop s))))))))
