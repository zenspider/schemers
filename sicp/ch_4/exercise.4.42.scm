#!/usr/bin/env csi -s

(require rackunit)
(use amb amb-extras)

;;; Exercise 4.42

;; Solve the following "Liars" puzzle (from Phillips 1934):
;;
;; Five schoolgirls sat for an examination. Their parents--so they
;; thought--showed an undue degree of interest in the result. They
;; therefore agreed that, in writing home about the examination, each
;; girl should make one true statement and one untrue one. The
;; following are the relevant passages from their letters:
;;
;;    * Betty : "Kitty was second in the examination. I was only third."
;;    * Ethel : "You'll be glad to hear that I was on top.  Joan was second."
;;    * Joan  : "I was third, and poor old Ethel was bottom."
;;    * Kitty : "I came out second.  Mary was only fourth."
;;    * Mary  : "I was fourth.  Top place was taken by Betty."
;;
;; What in fact was the order in which the five girls were placed?

(define (xor a b)
  (or (and (not b) a)
      (and (not a) b)))

(define (bitches)
  (define (declare self self-place other other-place)
    (amb-assert (xor (= self self-place) (= other other-place))))

  (let ((b (amb 1 2 3 4 5))
        (e (amb 1 2 3 4 5))
        (j (amb 1 2 3 4 5))
        (k (amb 1 2 3 4 5))
        (m (amb 1 2 3 4 5)))

    (amb-assert (distinct? (list b e j k m)))

    (declare b 3 k 2)
    (declare e 1 j 2)
    (declare j 3 e 5)
    (declare k 2 m 4)
    (declare m 4 b 1)

    (list (list 'b b)
          (list 'e e)
          (list 'j j)
          (list 'k k)
          (list 'm m))))

(all-of (bitches))

