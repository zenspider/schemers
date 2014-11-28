#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.70:

;; The following eight-symbol alphabet with associated relative
;; frequencies was designed to efficiently encode the lyrics of 1950s
;; rock songs. (Note that the "symbols" of an "alphabet" need not be
;; individual letters.)
;;
;;      A     2 NA   16
;;      BOOM  1 SHA  3
;;      GET   2 YIP  9
;;      JOB   2 WAH  1
;;
;; Use `generate-huffman-tree' (*Note Exercise 2-69::) to generate a
;; corresponding Huffman tree, and use `encode' (*Note Exercise
;; 2-68::) to encode the following message:
;;
;;      Get a job
;;      Sha na na na na na na na na
;;      Get a job
;;      Sha na na na na na na na na
;;      Wah yip yip yip yip yip yip yip yip yip
;;      Sha boom
;;
;; How many bits are required for the encoding?  What is the smallest
;; number of bits that would be needed to encode this song if we used
;; a fixed-length code for the eight-symbol alphabet?

;; (let ((input '((a 2) (na 16) (boom 1) (sha 3)
;;                (get 2) (yip 9) (job 2) (wah 1))))
;;   (let ((tree (generate-huffman-tree input))
;;         (song '(get a job
;;                     sha na na na na na na na na
;;                     get a job
;;                     sha na na na na na na na na
;;                     wah yip yip yip yip yip yip yip yip yip
;;                     sha boom)))
;;     (encode song tree)))

;; tree:
'((leaf na 16)
 ((leaf yip 9)
  ((((leaf boom 1) (leaf wah 1) (boom wah) 2) (leaf a 2) (boom wah a) 4)
   ((leaf sha 3) ((leaf get 2) (leaf job 2) (get job) 4) (sha get job) 7)
   (boom wah a sha get job)
   11)
  (yip boom wah a sha get job)
  20)
 (na yip boom wah a sha get job)
 36)

;; song:
'(1 1 1 1 0   1 1 0 1   1 1 1 1 1
    1 1 1 0   0   0   0   0   0   0   0   0
    1 1 1 1 0   1 1 0 1   1 1 1 1 1
    1 1 1 0   0   0   0   0   0   0   0   0
    1 1 0 0 1   1 0   1 0   1 0   1 0   1 0   1 0   1 0   1 0   1 0
    1 1 1 0   1 1 0 0 0)

(done)
