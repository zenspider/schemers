#lang RacketSchool/RecordsAll

(defun (f rec)
  (@ rec "a_field"))

(defun (g rec)
  (f rec))

(defun (h rec)
  (@ rec (++ "a_" "field")))

(defun (rec n)
  (record ("a_field" n)))

;; (@ (record ("1" 1)) "1")
;; (@ (record ("1" 1)) 1)

;; (f (record ("a_field" 1)))                ; 1 1 1
;; (@ (record ("a" 1) ("b" 2) ("c" 3)) "a")  ; 1 1 1
;; (g (record ("a_field" 1)))
                                             ; 1 1 1
;; (let ((f 42))
;;   (f (record ("a_field" 1))))             ; stuck

;; (h (record ("a_field" 1)))                ; stuck 1 1
;; (f (rec 2))                               ; 2 2 2

;; (let ([key (++ "a_" "field")])            ; 2 2 2
;;   (@ (rec 2) key))

;; (@ (record ("one" 1) ("2" 2)) "2")        ; 2 2 2
;; (@ (record ("one" 1) ("2" 2)) 2)          ; stuck stuck 2
