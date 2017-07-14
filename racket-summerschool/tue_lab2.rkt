#lang RacketSchool/Functions2

(defun (f x) (+ x 1))
(defun (g n) (+ 1 n))
(defun (i x) x)
(defun (l id) (function id))
(defun (not x) (if x false true))
(defun (dec n) (+ n -1))
(defun (c n)
  (if (zero? n)
      ""
      (++ "a" (c (+ n -1)))))
(defun (d n) (+ n n))


(defun (even? n)
  (if (zero? n)
      true
      (odd? (dec n))))

(defun (odd? n)
  (if (zero? n)
      false
      (even? (dec n))))

(c 5) ; "aaaaa", "", nohalt

;; (even? 0)                               ; t
;; (odd?  0)                               ; f
;; (even? 1)                               ; t
;; (odd?  1)                               ; t 0 t
;; (even? 50)                              ; t 49 nohalt
;; (odd? 50)                               ; f 49 nohalt
;; (even? 51)                              ; f 50 nohalt
;; (odd? 51)                               ; t 50 nohalt
;; (i (odd? 51))                           ; t 50 nohalt

;; (f (f 1))                               ; 3 2 3
;; (d 2)                                   ; 4 4 4
;; (d (d 2))                               ; 8 4 8
;; (d (d (d 2)))                           ; 16 4 16

;; language 1 -- normal
;; language 2 -- function calls are gotos (not gosubs). First return wins.
;; language 3 -- cobol ... recursion doesn't work. essentially no stack frame
