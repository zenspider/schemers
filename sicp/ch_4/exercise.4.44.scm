#!/usr/bin/env csi -s

(require rackunit)
(use amb amb-extras)

;;; Exercise 4.44

;; *Note Exercise 2-42:: described the "eight-queens puzzle" of
;; placing queens on a chessboard so that no two attack each other.
;; Write a nondeterministic program to solve this puzzle.

(define col cadr)
(define row car)

(define (safe? board)
  (or (null? board)
      (let ((cell (car board))
            (rest (cdr board)))
        (let ((r (row cell))
              (c (col cell)))
          (define (iterate board)
            (or (null? board)
                (let ((cell (car board))
                      (rest (cdr board)))
                  (let ((nr (row cell))
                        (nc (col cell)))
                    (and (not (= (abs (- nr r)) (abs (- nc c))))
                         (iterate rest))))))
          (and (not (member c (map col rest)))
               (not (member r (map row rest)))
               (iterate rest))))))

(define (royal-bitches8)
  (let* ((q1 (list 1 (amb 1 2 3 4 5 6 7 8)))
         (q2 (list 2 (amb 1 2 3 4 5 6 7 8)))
         (q3 (list 3 (amb 1 2 3 4 5 6 7 8)))
         (q4 (list 4 (amb 1 2 3 4 5 6 7 8)))
         (q5 (list 5 (amb 1 2 3 4 5 6 7 8)))
         (q6 (list 6 (amb 1 2 3 4 5 6 7 8)))
         (q7 (list 7 (amb 1 2 3 4 5 6 7 8)))
         (q8 (list 8 (amb 1 2 3 4 5 6 7 8)))
         (b  (list q1 q2 q3 q4 q5 q6 q7 q8)))

    (amb-assert (distinct? b))
    (amb-assert (safe? b))

    b))

(define (royal-bitches4)
  (let* ((q1 (list 1 (amb 1 2 3 4)))
         (q2 (list 2 (amb 1 2 3 4)))
         (q3 (list 3 (amb 1 2 3 4)))
         (q4 (list 4 (amb 1 2 3 4)))
         (b  (list q1 q2 q3 q4)))

    (amb-assert (distinct? b))
    (amb-assert (safe? b))

    b))

(test #t (safe? '((3 1) (1 2) (4 3) (2 4))))
(test #f (safe? '((1 1) (1 2) (1 3) (1 4))))
(test #f (safe? '((1 1) (1 2) (1 3) (4 4))))

(test  4 (length (all-of (royal-bitches4))))
;; (test 92 (length (all-of (royal-bitches8)))) ; too slow

;; (use extras)
;; (pp (all-of (royal-bitches)))           ; clearly wrong...
