#!/usr/bin/env csi -s

(use test)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.66

;; Ben has been generalizing the query system to
;; provide statistics about the company.  For example, to find the
;; total salaries of all the computer programmers one will be able to
;; say
;;
;;      (sum ?amount
;;           (and (job ?x (computer programmer))
;;                (salary ?x ?amount)))
;;
;; In general, Ben's new system allows expressions of the form
;;
;;      (accumulation-function <VARIABLE>
;;                             <QUERY PATTERN>)
;;
;; where `accumulation-function' can be things like `sum', `average',
;; or `maximum'.  Ben reasons that it should be a cinch to implement
;; this.  He will simply feed the query pattern to `qeval'.  This
;; will produce a stream of frames.  He will then pass this stream
;; through a mapping function that extracts the value of the
;; designated variable from each frame in the stream and feed the
;; resulting stream of values to the accumulation function.  Just as
;; Ben completes the implementation and is about to try it out, Cy
;; walks by, still puzzling over the `wheel' query result in exercise
;; *Note Exercise 4-65::.  When Cy shows Ben the system's response,
;; Ben groans, "Oh, no, my simple accumulation scheme won't work!"
;;
;; What has Ben just realized?  Outline a method he can use to
;; salvage the situation.

;; No clue what he just realized, but it can't be that hard, because
;; we did most of it via all-of:

(test 458000 (fold + 0 (map third (all-of '(salary ?who ?amount)))))

(define (accumulation-function fun identity var query)
  (let ((index (list-index (lambda (x) (eq? var x)) query)))
    (fold fun identity (map (lambda (l) (list-ref l index)) (all-of query)))))

(test 458000 (accumulation-function + 0 '?amount '(salary ?who ?amount)))

(test '((Bitdiddle Ben)
        (Hacker Alyssa P)
        (Fect Cy D)
        (Tweakit Lem E)
        (Reasoner Louis)
        (Warbucks Oliver)
        (Scrooge Eben)
        (Cratchet Robert)
        (Aull DeWitt))
      (accumulation-function cons '() '?who '(salary ?who ?amount)))

;; maybe he's just a bad programmer?

;; I'd call it stream-map-fold personally
