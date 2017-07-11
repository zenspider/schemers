#lang racket

(require redex)

(define-language peg-solitaire
  [position ::= █ ○ ●]
  [board    ::= ([position ...] ...)])

(define-term initial-board
  ([█ █ ● ● ● █ █]
   [█ █ ● ● ● █ █]
   [● ● ● ● ● ● ●]
   [● ● ● ○ ● ● ●]
   [● ● ● ● ● ● ●]
   [█ █ ● ● ● █ █]
   [█ █ ● ● ● █ █]))

;; puzzlescript.net

(define move
  (reduction-relation
   peg-solitaire
   #:domain board
   (--> (any_1 ... [any_2 ... ● ● ○ any_3 ...] any_4 ...)
        (any_1 ... [any_2 ... ○ ○ ● any_3 ...] any_4 ...)
        →)
   (--> (any_1 ... [any_2 ... ○ ● ● any_3 ...] any_4 ...)
        (any_1 ... [any_2 ... ● ○ ○ any_3 ...] any_4 ...)
        ←)
   (--> (any_1 ...
         [any_2 ..._1 ● any_3 ...]
         [any_4 ..._1 ● any_5 ...]
         [any_6 ..._1 ○ any_7 ...]
         any_8 ...)
        (any_1 ...
         [any_2 ... ○ any_3 ...]
         [any_4 ... ○ any_5 ...]
         [any_6 ... ● any_7 ...]
         any_8 ...)
        ↓)
   (--> (any_1 ...
         [any_2 ..._1 ○ any_3 ...]
         [any_4 ..._1 ● any_5 ...]
         [any_6 ..._1 ● any_7 ...]
         any_8 ...)
        (any_1 ...
         [any_2 ... ● any_3 ...]
         [any_4 ... ○ any_5 ...]
         [any_6 ... ○ any_7 ...]
         any_8 ...)
        ↑)))

(apply-reduction-relation move (term initial-board))

;; (stepper move (term initial-board))

;; (traces move (term initial-board))

(define (in-lists lists) (apply in-sequences lists)) ; simple flattened stream

(define (winning? board)
  (= 1 (stream-count (curry eq? '●) (in-lists board))))

(define (search-for-solution board)
  (define (step board-with-move)
    (match-define `(,_ ,board) board-with-move)
    (define next-boards-with-moves
      (apply-reduction-relation/tag-with-names move board))
    (cond
      [(empty? next-boards-with-moves)
       (and (winning? board) `(,board-with-move))]
      [else
       (define rest-of-solution
         (ormap step next-boards-with-moves))
       (and rest-of-solution
            `(,board-with-move ,@rest-of-solution))]))
  (step `("initial" ,board)))

(search-for-solution (term ([● ● ○])))
(search-for-solution (term ([● ● ○ ●])))
(search-for-solution (term ([● ● ● ○])))
