
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 2.53:

;; What would the interpreter print in response to evaluating each of
;; the following expressions?
;;
;;      (list 'a 'b 'c)
;;
;;      (list (list 'george))
;;
;;      (cdr '((x1 x2) (y1 y2)))
;;
;;      (cadr '((x1 x2) (y1 y2)))
;;
;;      (pair? (car '(a short list)))
;;
;;      (memq 'red '((red shoes) (blue socks)))
;;
;;      (memq 'red '(red shoes blue socks))

(assert-equal '(a b c)                (list 'a 'b 'c))
(assert-equal '((george))             (list (list 'george)))
(assert-equal '((y1 y2))              (cdr '((x1 x2) (y1 y2))))
(assert-equal '(y1 y2)                (cadr '((x1 x2) (y1 y2))))
(assert-equal #f                      (pair? (car '(a short list))))
(assert-equal #f                      (memq 'red '((red shoes) (blue socks))))
(assert-equal '(red shoes blue socks) (memq 'red '(red shoes blue socks)))

(done)
