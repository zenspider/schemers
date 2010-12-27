#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;; Exercise 2.42

;; *Figure 2.8:* A solution to the eight-queens puzzle.
;;
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   | Q |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   | Q |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      | Q |   |   |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   |   | Q |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   | Q |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   |   |   |   |   | Q |
;;      +---+---+---+---+---+---+---+---+
;;      |   | Q |   |   |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;      |   |   |   | Q |   |   |   |   |
;;      +---+---+---+---+---+---+---+---+
;;
;; The "eight-queens puzzle" asks how to place eight queens on a
;; chessboard so that no queen is in check from any other (i.e., no
;; two queens are in the same row, column, or diagonal). One possible
;; solution is shown in *Note Figure 2-8::. One way to solve the
;; puzzle is to work across the board, placing a queen in each column.
;; Once we have placed k - 1 queens, we must place the kth queen in a
;; position where it does not check any of the queens already on the
;; board. We can formulate this approach recursively: Assume that we
;; have already generated the sequence of all possible ways to place k
;; - 1 queens in the first k - 1 columns of the board. For each of
;; these ways, generate an extended set of positions by placing a
;; queen in each row of the kth column. Now filter these, keeping only
;; the positions for which the queen in the kth column is safe with
;; respect to the other queens. This produces the sequence of all ways
;; to place k queens in the first k columns. By continuing this
;; process, we will produce not only one solution, but all solutions
;; to the puzzle.
;;
;; We implement this solution as a procedure `queens', which returns a
;; sequence of all solutions to the problem of placing n queens on an
;; n*n chessboard.  `Queens' has an internal procedure `queen-cols'
;; that returns the sequence of all ways to place queens in the first
;; k columns of the board.
;;
;;      (define (queens board-size)
;;        (define (queen-cols k)
;;          (if (= k 0)
;;              (list empty-board)
;;              (filter
;;               (lambda (positions) (safe? k positions))
;;               (flatmap
;;                (lambda (rest-of-queens)
;;                  (map (lambda (new-row)
;;                         (adjoin-position new-row k rest-of-queens))
;;                       (enumerate-interval 1 board-size)))
;;                (queen-cols (- k 1))))))
;;        (queen-cols board-size))
;;
;; In this procedure `rest-of-queens' is a way to place k - 1 queens
;; in the first k - 1 columns, and `new-row' is a proposed row in
;; which to place the queen for the kth column.  Complete the program
;; by implementing the representation for sets of board positions,
;; including the procedure `adjoin-position', which adjoins a new
;; row-column position to a set of positions, and `empty-board',
;; which represents an empty set of positions.  You must also write
;; the procedure `safe?', which determines for a set of positions,
;; whether the queen in the kth column is safe with respect to the
;; others.  (Note that we need only check whether the new queen is
;; safe--the other queens are already guaranteed safe with respect to
;; each other.)

(define empty-board null)
(define col cadr)
(define row car)

(define (safe? ignored board)
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

(define (adjoin-position r c rest-of-queens)
  (cons (list r c) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))

(assert-equal '((r1 c1))         (adjoin-position 'r1 'c1 null))
(assert-equal '((r2 c2) (r1 c1)) (adjoin-position 'r2 'c2 '((r1 c1))))

(let ((_ null))
  (assert (safe? _ '((3 1) (1 2) (4 3) (2 4))))
  (refute (safe? _ '((1 1) (1 2) (1 3) (1 4))))
  (refute (safe? _ '((1 1) (1 2) (1 3) (4 4)))))

(assert-equal '(1 0 0 2 10 4 40 92)
              (map (lambda (n) (length (queens n)))
                   (enumerate-interval 1 8)))

(done)
