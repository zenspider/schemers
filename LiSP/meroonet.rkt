#lang racket

;; (define *maximal-number-of-classes* 100)
;; (define *classes*                   (make-vector *maximal-number-of-classes* #f))
;; (define *class-number*              0)
;; (define *generics*                  (list))
;; 
;; (define (number->class n)
;;   (vector-ref *classes* n))
;; 
;; (define (->Class name)
;;   (let scan ((index (- *class-number* 1)))
;;     (and (>= index 0)
;;          (let ((c (vector-ref *classes* index)))
;;            (if (eq? name (Class-name c))
;;                c
;;                (scan (- index 1)))))))
;; 
;; (define (->Generic name)
;;   (let lookup ((l *generics))
;;     (if (pair? l)
;;         (if (eq? name (Generic-name (car l)))
;;             (car l)
;;             (lookup (cdr l)))
;;         #f)))
