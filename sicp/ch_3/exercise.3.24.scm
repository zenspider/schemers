#!/usr/local/bin/csi -s

(use test)

;;; Exercise 3.24

;; In the table implementations above, the keys are
;; tested for equality using `equal?' (called by `assoc').  This is
;; not always the appropriate test.  For instance, we might have a
;; table with numeric keys in which we don't need an exact match to
;; the number we're looking up, but only a number within some
;; tolerance of it.  Design a table constructor `make-table' that
;; takes as an argument a `same-key?' procedure that will be used to
;; test "equality" of keys.  `Make-table' should return a `dispatch'
;; procedure that can be used to access appropriate `lookup' and
;; `insert!' procedures for a local table.

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

(define (get k t)   ((t 'lookup-proc) k))
(define (put k v t) ((t 'insert-proc) k v))

;; NOTE:
;; (optional var default-value) is a macro for:
;;
;; (or (and (not (null? var)) (car var)) default-value))

(test-group "3.24"
            (define t (make-table eq?))

            (test #f (get 'x t))

            (put 'x 42 t)
            (test 42 (get 'x t))

            (put 'x 24 t)
            (test 24 (get 'x t)))
