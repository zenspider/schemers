#lang racket

(provide accumulate
         accumulate-n
         inc
         square)

(define (inc n) (+ 1 n))

(define (square n) (* n n))

(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs)) null
      (cons (accumulate   op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; (define (flatmap proc seq)
;;   (accumulate append null (map proc seq)))
;; (define (enumerate-interval low high)
;;   (if (> low high)
;;       null
;;       (cons low (enumerate-interval (+ low 1) high))))
;;
;; (define (enumerate-tree tree)
;;   (cond ((null? tree) null)
;;         ((not (pair? tree)) (list tree))
;;         (else (append (enumerate-tree (car tree))
;;                       (enumerate-tree (cdr tree))))))
;;
;; (define (sum-odd-squares tree)
;;   (accumulate +
;;               0
;;               (map square
;;                    (filter odd?
;;                            (enumerate-tree tree)))))
;;
;; (assert-equal 15             (accumulate + 0 '(1 2 3 4 5)))
;; (assert-equal 120            (accumulate * 1 '(1 2 3 4 5)))
;; (assert-equal '(1 2 3 4 5)   (accumulate cons null '(1 2 3 4 5)))
;; (assert-equal '(2 3 4 5 6 7) (enumerate-interval 2 7))
;; (assert-equal '(1 2 3 4 5)   (enumerate-tree '(1 (2 (3 4)) 5)))
;; (assert-equal 35             (sum-odd-squares '(1 2 3 4 5)))

