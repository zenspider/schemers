
(require-extension test)

;;; Exercise 3.23

;; A "deque" ("double-ended queue") is a sequence in
;; which items can be inserted and deleted at either the front or the
;; rear.  Operations on deques are the constructor `make-deque', the
;; predicate `empty-deque?', selectors `front-deque' and
;; `rear-deque', and mutators `front-insert-deque!',
;; `rear-insert-deque!', `front-delete-deque!', and
;; `rear-delete-deque!'.  Show how to represent deques using pairs,
;; and give implementations of the operations.(2)  All operations
;; should be accomplished in [theta](1) steps.

(define null '())

;; head         tail
;; |            |
;; [*|*] [*|*] [*|*]
;;  | |   | |   | |
;;  a |   b |   c |
;;    |     |     |
;;   [0|b] [a|c] [b|0]

;; NOTE: I like the OO-ness of this form of programming... However,
;; during development it is increasingly hard to use inside the REPL.
;; Any implementation change to the module requires redefining all of
;; make-deque, but that doesn't change any existing objects that are
;; from previous versions. They're all bound to old functions.

(define (make-deque)
  (let ((head '())
        (tail '()))

    (define (empty?)
      (null? head))

    ;; unshift a cell onto the front of the deque
    (define (unshift x)
      (if (empty?)
          (let ((cell (cons x (list null null))))
            (set! head cell)
            (set! tail cell))
          (let ((cell (cons x (list null head))))
            (set-car! (cdr head) cell)
            (set! head cell))))

    ;; shift a cell from the front of the deque
    (define (shift)
      (cond ((empty?)
             (error "DELETE! called with an empty queue" head))
            (else
             (set! head (cddr head))
             (if (null? head)
                 (set! tail null)
                 (set-car! (cdr head) null)))))

    ;; push a cell onto the end of the deque
    (define (push x)
      (if (empty?)
          (let ((cell (cons x (list null null))))
            (set! head cell)
            (set! tail cell))
          (let ((cell (cons x (list tail null))))
            (set-cdr! (cdr tail) cell)
            (set! tail cell))))

    ;; pop a cell from the end of the deque
    (define (pop)
      (cond ((empty?)
             (error "DELETE! called with an empty queue" head))
            (else
             (set! tail (cadr tail))
             (if (null? tail)
                 (set! head null)
                 (set-cdr! (cdr tail) null)))))

    (define (dispatch m)
      (cond ((eq? m 'head)    head)
            ((eq? m 'tail)    tail)
            ((eq? m 'empty?)  empty?)
            ((eq? m 'unshift) unshift)
            ((eq? m 'push)    push)
            ((eq? m 'shift)   shift)
            ((eq? m 'pop)     pop)
            (else (error "bad msg: " m))))

    dispatch))

(define (head-deque q)            (q 'head))
(define (tail-deque q)            (q 'tail))
(define (empty-deque? q)         ((q 'empty?)))
(define (head-insert-deque! q x) ((q 'unshift) x))
(define (tail-insert-deque! q x) ((q 'push) x))
(define (head-delete-deque! q)   ((q 'shift)))
(define (tail-delete-deque! q)   ((q 'pop)))

(test-group "3.23"
            (define q (make-deque))

            ;; ()
            (test #t (empty-deque? q))
            (test null (head-deque q))
            (test null (tail-deque q))

            ;; (a)
            (head-insert-deque! q 'a)
            (test 'a (car (head-deque q)))
            (test 'a (car (tail-deque q)))

            ;; (a b)
            (tail-insert-deque! q 'b)
            (test 'a (car (head-deque q)))
            (test 'b (car (tail-deque q)))

            ;; (a b c)
            (display-deque q)
            (newline)
            (tail-insert-deque! q 'c)
            (test 'a (car (head-deque q)))
            (test 'c (car (tail-deque q)))

            ;; (b c)
            (head-delete-deque! q)
            (test 'b (car (head-deque q)))
            (test 'c (car (tail-deque q)))

            ;; (b)
            (tail-delete-deque! q)
            (test 'b (car (head-deque q)))
            (test 'b (car (tail-deque q)))

            ;; ()
            (head-delete-deque! q)
            (test #t (empty-deque? q))
            (test null (head-deque q))
            (test null (tail-deque q)))
