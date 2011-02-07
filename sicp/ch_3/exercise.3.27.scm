
(use test)

;;; Exercise 3.27

;; "Memoization" (also called "tabulation") is a
;; technique that enables a procedure to record, in a local table,
;; values that have previously been computed.  This technique can
;; make a vast difference in the performance of a program.  A memoized
;; procedure maintains a table in which values of previous calls are
;; stored using as keys the arguments that produced the values.  When
;; the memoized procedure is asked to compute a value, it first
;; checks the table to see if the value is already there and, if so,
;; just returns that value.  Otherwise, it computes the new value in
;; the ordinary way and stores this in the table.  As an example of
;; memoization, recall from section *Note 1-2-2:: the exponential
;; process for computing Fibonacci numbers:

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; With the memoizer defined as:

(define (make-table . test)
  (let ((local-table (list '*table*))
        (same-key? (optional test equal?))) ;; see note below

    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons key value) (cdr local-table))))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert)))

    dispatch))

(define (lookup  k t)   ((t 'lookup-proc) k))
(define (insert! k v t) ((t 'insert-proc) k v))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;; The memoized version of the same procedure is:

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;; Draw an environment diagram to analyze the computation of
;; `(memo-fib 3)'.

;; E0:
;;   fib: ...
;;   memoize: ...
;;   memo-fib: E1
;;
;; E1:
;;   f: ...
;;   table: *-------*---*---*--0
;;          |       |   |   |
;;          *table* 0:0 1:1 2:2

;; Explain why `memo-fib' computes the nth Fibonacci number in a
;; number of steps proportional to n.

;; A: it is always computing (memo-fib N) as (get (- N 2)) + (get (- N 1))

;; Would the scheme still work if we had simply defined `memo-fib' to
;; be `(memoize fib)'?

;; A: not as written, since the computation needs to recurse on itself.

(test-group "3.27"
            (test 6765 (fib 20))
            (test 6765 (memo-fib 20)))

;; (fib 30) vs (memo-fib 30):
;; procedure     calls  seconds  average  percent
;; ----------------------------------------------
;; fib         2692537    6.958    0.000  100.000
;; lookup           59    0.000    0.000    0.000
;; insert!          31    0.000    0.000    0.000
;; memoize           1    0.000    0.000    0.000
;; make-table        1    0.000    0.000    0.000
