#lang racket

(module+ test
  (require rackunit))

(require racket/syntax)
(require syntax/parse/define)
(require json)
(require net/url)

(define-simple-macro (define/case name e ...) (define name (case-lambda e ...)))
(define-simple-macro (for/none e ...)         (not (for/or e ...)))
(define-simple-macro (nor e ...)              (not (or e ...)))

(define dot '|.|)
(define hit  '●)
(define miss '○)

(struct board (b) #:transparent #:extra-constructor-name -board)
(struct ship (name) #:transparent)

(define battleship (ship 'battleship))
(define cruiser    (ship 'cruiser))
(define submarine  (ship 'submarine))
(define frigate    (ship 'frigate))
(define destroyer  (ship 'destroyer))

(define fleet (list battleship cruiser submarine frigate destroyer))

(define rows '(A B C D E F G H I J))
(define cols (range 1 11))

(define (->coord r c)
  (format-symbol "~a~a" r c))

(define (rc->coord rc)
  (->coord (list-ref rows (sub1 (first rc))) (second rc)))

(module+ test
  (check-equal? (->coord 'A 1) 'A1))

(define coordinates
  (for*/list ([r (in-list rows)]
              [c (in-list cols)])
    (->coord r c)))

(module+ test
  (check-equal? (take coordinates 3) '(A1 A2 A3)))

(module+ test
  (let ([b (new-game (hash submarine '(A1 A2 A3)
                           frigate   '(A4 A5)))])
    (check-equal? (board-ref b 'A 1) submarine)
    (check-equal? (board-ref b 'A 4) frigate)))

(define (new-game [placements (hash)])
  (define board (-board (hash)))
  (for/fold ([board board])
            ([(ship places) (in-hash placements)])
    (place-ship board ship places)))

(define (in-board board)
  (in-hash (board-b board)))

(define/case board-ref
  [(board r c) (board-ref board (->coord r c))]
  [(board rc)  (hash-ref (board-b board)
                         (if (symbol? rc) rc (string->symbol rc))
                         null)])

(module+ test
  (check-equal? (board-ref (new-game) 'A 1) null)
  (check-equal? (board-ref (new-game) 'A1)  null))

(define (place-ship board ship coords)
  (for/fold ([board board])
            ([coord (in-list coords)])
    (-board (hash-set (board-b board) coord ship))))

(define (board-set board value coord)
  (place-ship board value (list coord)))

(module+ test
  (let ([b (place-ship (new-game) submarine '(A1 A2 A3))])
   (check-equal? (board-ref b 'A 1) submarine)
   (check-equal? (board-ref b 'A 2) submarine)
   (check-equal? (board-ref b 'A 4) null)))

(define (->marker val)
  (match val
    [(ship name) (string->symbol (substring (symbol->string name) 0 1))]
    [#t          hit]
    [#f          miss]
    ['()         dot]))

(module+ test
  (check-equal? (->marker submarine) 's)
  (check-equal? (->marker null)      dot)
  (check-equal? (->marker #t)        hit)
  (check-equal? (->marker #f)        miss))

(define (board->string board)
  (string-join
   (for/list ([r (in-list rows)])
     (string-join
      (for/list ([c (in-list cols)])
        (symbol->string (->marker (board-ref board r c))))))
   "\n"))

(module+ test
  (check-equal? (board->string (new-game))
                (string-join
                 (for/list ([r (in-list rows)])
                   ". . . . . . . . . .")
                 "\n"))
  (check-equal? (board->string (new-game (hash submarine '(A1 A2 A3)
                                               #t '(A4)
                                               #f '(A5))))
                (string-join
                 (cons
                  "s s s ● ○ . . . . ."
                  (for/list ([r (in-list (rest rows))])
                    ". . . . . . . . . ."))
                 "\n")))

(define (board-print board)
  (displayln (board->string board)))

(define (game-live? board)
  (for/or ([(rc v) (in-board board)]
           #:when (ship? v))
    #t))

(module+ test
  (check-equal? (game-live? (new-game))                        #f)
  (check-equal? (game-live? (new-game (hash submarine '(A1)))) #t))

(define neighbors
  (for*/hash ([r (in-range 1 11)]
              [c (in-list cols)])
    (define n (sub1 r))
    (define s (add1 r))
    (define w (sub1 c))
    (define e (add1 c))
    (define coords (filter (λ (l) (and (< 0 (first l) 11) (< 0 (second l) 11)))
                           (list (list n c)
                                 (list s c)
                                 (list r w)
                                 (list r e))))
    (values (rc->coord (list r c))
            (for/list ([coord (in-list coords)])
              (rc->coord coord)))))

(define (board->scores board)
  (for/hash ([(rc v) (in-board board)])
    (values rc
            (cond
              [(null? v) -0.1]
              [v 1]
              [else 0]))))

(define (calculate-moves board moves)
  (define scores (board->scores board))
  (map car
       (sort (hash->list
              (for/hash ([rc (in-list moves)])
                (let* ([coords (hash-ref neighbors rc)]
                       [score (/ (for/sum ([coord (in-list coords)])
                                   (hash-ref scores coord 0))
                                 (length coords))])
                  (values rc score))))
             (λ (a b) (< (cdr b) (cdr a))))))

(define (fire board coordinate)
  (define s (board-ref board coordinate))
  (if (ship? s)
      (let* ([nb   (board-set board #t coordinate)]
             [sunk (and (for/none ([(rc v) (in-board nb)]
                                   #:when (ship? v))
                          (equal? s v))
                        (symbol->string (ship-name s)))]
             [dead (not (for/or ([(rc v) (in-board nb)])
                          (ship? v)))])
        (values nb #t sunk dead))
      (values (board-set board #f coordinate) #f #f #f)))

(define-simple-macro (deval expr)
  (call-with-values (thunk expr) list))

(module+ test
  (define-check (check-fire places exp)
    (let-values ([(b h s d) (fire (new-game (hash submarine places)) 'A1)])
      (check-equal? (list (board-ref b 'A1) h s d)
                    exp)))
  (check-fire '()      '(#f #f #f #f))        ; miss
  (check-fire '(A1)    '(#t #t submarine #t)) ; hit/sunk/dead
  (check-fire '(A1 A2) '(#t #t #f #f)))       ; hit/live

(define (post uri payload)
  (string->jsexpr
   (port->string (post-pure-port (string->url uri)
                                 (jsexpr->bytes payload)))))

(define (get uri)
  (string->jsexpr
   (port->string (get-pure-port (string->url uri)))))

(define (dig hash . keys)
  (for/fold ([hash hash])
            ([key (in-list keys)])
    (let ([v (hash-ref hash key '#hash())])
      (if (string? v)
          (string->symbol v)
          v))))

(module+ test
  #;(check-equal? #t #f)
  )

(define (next-move game-id turn-id coordinate) ; rename to payload?
  (hash 'game_id game-id
        'guess (hash 'guess (symbol->string coordinate)
                     'turn_id turn-id)))

(module+ test
  #;(check-equal? #t #f)
  )

(define (my-result payload turn-id hit? sunk? lost?)
  (hash-set payload 'response (hash 'hit hit?
                                    'sunk sunk?
                                    'lost lost?
                                    'turn_id turn-id)))

(module+ test
  #;(check-equal? #t #f))

(define game-uri "https://battleship-176302.appspot.com/new_game")
(define turn-uri "https://battleship-176302.appspot.com/turn")

;; { game_id: <id>,
;;   response: { hit: [true|false], sunk: <ship name>, turn_id: i },
;;   guess:    { guess: "A7", turn_id: i + 1 } }

(define placement (hash battleship '(A8 B8 C8 D8 E8)
                        cruiser    '(G8 H8 I8 J8)
                        submarine  '(F1 F2 F3)
                        frigate    '(F5 F6 F7)
                        destroyer  '(F9 F10)))

(define (play placement)
  ;; me                                     server
  ;; ---------------------------------------------
  ;; 1. initialize boards & moves
  ;;
  ;; +-> 2. calculate move and/or response
  ;; |
  ;; |   3. post payload -----------------> /turn
  ;; |                    <---------------- response
  ;; |
  ;; |   4. extract response & update theirs
  ;; |   5. extract guess & update mine
  ;; |
  ;; +-- 6. repeat w/ new moves

  ;; 1. initialize boards & moves

  (define game-id (dig (get game-uri) 'game_id))
  ;; (define game-id 30)

  (printf "game-id: ~a~n" game-id)

  (let loop ([turn-id 1]
             [mine (new-game placement)]   ; boards
             [theirs (new-game)]
             [moves (shuffle coordinates)]
             [mine-hit? #f]
             [mine-sunk? #f]
             [mine-dead? #f])

    (printf "Turn ~a~n" turn-id)
    (board-print theirs)
    (newline)
    (board-print mine)
    (newline)

    ;; 2. calculate move and/or response

    (define my-move (first moves))

    ;; 3. post payload
    (define payload (my-result (next-move game-id turn-id my-move)
                               (sub1 turn-id)
                               mine-hit?
                               mine-sunk?
                               mine-dead?))
    (define response (post turn-uri payload))

    ;; 4. extract response & update theirs
    (define their-hit?  (dig response 'response 'hit))
    (define their-dead? (dig response 'response 'lost))
    (set! theirs (board-set theirs their-hit? my-move))

    ;; 5. extract guess & update mine
    (define their-guess (dig response 'guess    'guess))
    (set!-values (mine mine-hit? mine-sunk? mine-dead?) (fire mine their-guess))

    (when (nor their-dead? mine-dead?)
      (loop (add1 turn-id)
            mine
            theirs
            (calculate-moves theirs (rest moves))
            mine-hit?
            mine-sunk?
            mine-dead?))

    (cond
      [their-dead? (displayln "You won!")]
      [mine-dead?  (displayln "You lost!")])))

(play placement)

(module+ test
  (displayln 'done))
