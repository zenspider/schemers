;;; Exercise 3.50

;; Complete the following definition, which
;; generalizes `stream-map' to allow procedures that take multiple
;; arguments, analogous to `map' in section *Note 2-2-3::, footnote
;; *Note Footnote 12::.
;;
;;      (define (stream-map proc . argstreams)
;;        (if (<??> (car argstreams))
;;            the-empty-stream
;;            (<??>
;;             (apply proc (map <??> argstreams))
;;             (apply stream-map
;;                    (cons proc (map <??> argstreams))))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

