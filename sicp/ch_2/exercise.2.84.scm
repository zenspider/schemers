(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.84

;; Using the `raise' operation of *Note Exercise
;; 2-83::, modify the `apply-generic' procedure so that it coerces
;; its arguments to have the same type by the method of successive
;; raising, as discussed in this section.  You will need to devise a
;; way to test which of two types is higher in the tower.  Do this in
;; a manner that is "compatible" with the rest of the system and will
;; not lead to problems in adding new levels to the tower.

(define tower '(integer rational real complex))

(define (subtype? x y)
  (and (not (equal? x y))
       (let ((supers (member x tower)))
         (and supers (member y supers)))))

(assert-equal '(complex) (subtype? 'integer 'complex))
(assert-equal #f         (subtype? 'complex 'integer))
(assert-equal #f         (subtype? 'integer 'integer))
(assert-equal #f         (subtype? 'complex 'complex))

;; (define (raise-from-to x y)
;;   (if (subtype? (tag-type x) (tag-type y))
;;       (raise-from-to (raise x) y)
;;       x))

;; the rest is just hooking it in. Unfortunately the direction I took
;; with my last converter was too clever to make that a simple pop-in.
