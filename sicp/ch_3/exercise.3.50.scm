
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

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

;; (assert-equal x y)
(done)
