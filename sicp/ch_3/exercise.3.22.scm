
(require-extension test)

;;; Exercise 3.22

;; Instead of representing a queue as a pair of
;; pointers, we can build a queue as a procedure with local state.
;; The local state will consist of pointers to the beginning and the
;; end of an ordinary list.  Thus, the `make-queue' procedure will
;; have the form
;;
;;      (define (make-queue)
;;        (let ((front-ptr ... )
;;              (rear-ptr ... ))
;;          <DEFINITIONS OF INTERNAL PROCEDURES>
;;          (define (dispatch m) ...)
;;          dispatch))
;;
;; Complete the definition of `make-queue' and provide
;; implementations of the queue operations using this representation.

(define (make-queue)
  (let ((head '())
        (tail '()))

    (define (set-rear-ptr! item) ;; TODO: remove
      (set! tail item))

    (define (empty?)
      (null? head))

    (define (push x)
      (let ((new-pair (cons x '())))
        (if (empty?)
            (begin (set! head new-pair)
                   (set! tail new-pair))
            (begin
              (set-cdr! tail new-pair)
              (set! tail new-pair)))))

    (define (pop)
      (cond ((empty?)
             (error "DELETE! called with an empty queue" head))
            (else
             (set! head (cdr head)))))

    (define (print)
      (display head))

    (define (dispatch m)
      (cond ((eq? m 'head) head)
            ((eq? m 'tail) tail)
            ((eq? m 'empty?) (empty?))
            ((eq? m 'push) push)
            ((eq? m 'pop) (pop))
            ((eq? m 'print) (print))
            (else (error "unknown command: " m))))
    dispatch))

(define (head q)    (q 'head))
(define (tail q)    (q 'tail))
(define (empty? q)  (q 'empty?))
(define (push q x) ((q 'push) x))
(define (pop q)     (q 'pop))
(define (print-queue q) (q 'print))

(define q (make-queue))
(test '() (head q))
(push q 'a)     ; => (a)
(test '(a) (head q))
(push q 'b)     ; => (a b)
(test '(a b) (head q))
(pop q)        ; => (b)
(test '(b) (head q))
(pop q)        ; => ()
(test '() (head q))
