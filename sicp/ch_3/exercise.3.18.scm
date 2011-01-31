#!/usr/local/bin/csi -s

(require-extension test)

;;; Exercise 3.18

;; Write a procedure that examines a list and determines whether it
;; contains a cycle, that is, whether a program that tried to find the
;; end of the list by taking successive `cdr's would go into an
;; infinite loop. *Note Exercise 3-13:: constructed such lists.

(define cdr-cycle '(a b c))
(set-cdr! (cddr cdr-cycle) cdr-cycle)

(define car-cycle '(a b c))
(set-car! car-cycle car-cycle)

(define (cyclic? x)
  (let ((visited '()))
    (define (iterate l)
      (define (test f)
        (let ((cell (f l)))
          (and (pair? cell)
               (or (memq cell visited)
                   (begin
                     (set! visited (cons cell visited))
                     (and (iterate cell) #t))))))
      (if (null? l) #f
          (or (test car)
              (test cdr))))
    (iterate x)))

(test-group "3.18"
            (test #f (cyclic? '(a b c)))
            (test #t (cyclic? car-cycle))
            (test #t (cyclic? cdr-cycle)))
