#!/usr/local/bin/csi -s

(use test)

(define length
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (length (cdr l)))))))

(test 0 (length '()))
(test 1 (length '(a)))
(test 2 (length '(a b)))

(define eternity
  (lambda (x) (eternity x)))

(define test-length
  (lambda (name f0 f1 f2)
    (test-group name
      (test 0 (f0 '()))

      (test 0 (f1 '()))
      (test 1 (f1 '(a)))

      (test 0 (f2 '()))
      (test 1 (f2 '(a)))
      (test 2 (f2 '(a b))))))

(define length0
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 (eternity (cdr l)))))))

(define length1
  (lambda (l)
    (cond ((null? l) 0)
          (else (add1 ((lambda (l)      ; length0
                         (cond ((null? l) 0)
                               (else (add1 (eternity (cdr l)))))) (cdr l)))))))

(define length2
  (lambda (l)
    (cond
     ((null? l) 0)
     (else
      (add1
       ((lambda (l)                     ; length0
          (cond
           ((null? l) 0)
           (else
            (add1
             ((lambda (l)
                (cond
                 ((null? l) 0)
                 (else
                  (add1 (eternity (cdr l)))))) (cdr l))))))
        (cdr l)))))))

(test-length "length" length0 length1 length2)

(define meta-length0
  ((lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))
   eternity))

;; dupe and replace eternity with itself

(define meta-length1
  ((lambda (f)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (f (cdr l)))))))
   ((lambda (g)
      (lambda (l)
        (cond ((null? l) 0)
              (else (add1 (g (cdr l)))))))
    eternity)))

;; ditto

(define meta-length2
  ((lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))
   ((lambda (length)
      (lambda (l)
        (cond ((null? l) 0)
              (else (add1 (length (cdr l)))))))
    ((lambda (length)
       (lambda (l)
         (cond ((null? l) 0)
               (else (add1 (length (cdr l)))))))
     eternity))))

(test-length "meta-length" meta-length0 meta-length1 meta-length2)

;; pull out the eternity repetition:

(define meta2-length0
  ((lambda (mk-length)
     (mk-length eternity))
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))

(define meta2-length1
  ((lambda (mk-length)
     (mk-length (mk-length eternity)))
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))

(define meta2-length2
  ((lambda (mk-length)
     (mk-length (mk-length (mk-length eternity))))
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))

(test-length "meta2-length" meta2-length0 meta2-length1 meta2-length2)

;; pass mk-length to itself instead of passing eternity:

(define meta3-length0
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (mk-length (cdr l)))))))))

(define meta3-length1
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 ((mk-length eternity) (cdr l)))))))))

(define meta3-length2 ; I cheat here, because I can't figger it out w/ eternity
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 ((mk-length mk-length) (cdr l)))))))))

(test-length "meta3-length" meta3-length0 meta3-length1 meta3-length2)

;; (((lambda (mk-length) (mk-length mk-length))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (if (null? l) 0
;;           (add1 ((mk-length eternity) (cdr l)))))))
;;  '(apples))

;; (((lambda (mk-length)
;;     (lambda (l)
;;       (if (null? l) 0
;;           (add1 ((mk-length eternity) (cdr l))))))
;;   (lambda (mk-length)
;;     (lambda (l)
;;       (if (null? l) 0
;;           (add1 ((mk-length eternity) (cdr l)))))))
;;  '(apples))

;; ((lambda (l)
;;    (if (null? l) 0
;;        (add1 (((lambda (mk-length)
;;                  (lambda (l)
;;                    (if (null? l) 0
;;                        (add1 ((mk-length eternity) (cdr l))))))
;;                eternity)
;;               (cdr l)))))
;;  '(apples))

;; (let ((l '(apples)))
;;   (if (null? l) 0
;;       (add1 (((lambda (mk-length)
;;                 (lambda (l)
;;                   (if (null? l) 0
;;                       (add1 ((mk-length eternity) (cdr l))))))
;;               eternity)
;;              (cdr l)))))

;; (let ((l '(apples)))
;;   (add1 (((lambda (mk-length)
;;             (lambda (l)
;;               (if (null? l) 0
;;                   (add1 ((mk-length eternity) (cdr l))))))
;;           eternity)
;;          (cdr l))))

;; (let ((l '(apples)))
;;   (add1 (((lambda (mk-length)
;;             (lambda (l)
;;               (if (null? l) 0
;;                   (add1 ((mk-length eternity) (cdr l))))))
;;           eternity)
;;          (cdr l))))

;; (let ((l '(apples)))
;;   (add1 (((lambda (l)
;;             (if (null? l) 0
;;                 (add1 ((eternity eternity) (cdr l)))))) (cdr l))))

;; (let ((l '(apples)))
;;   (add1 (let ((l2 (cdr l)))
;;           (if (null? l2) 0
;;               (add1 ((eternity eternity) (cdr l2)))))))

;; (let ((l '(apples)))
;;   (add1 (let ((l2 (cdr l)))
;;           0)))

;; (add1 0)

(define meta4-length
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 ((mk-length mk-length) (cdr l)))))))))

(test-length "meta4-length" meta4-length meta4-length meta4-length)

;; extract (mk-length mk-length) to recreate original length function:
;;
;;   (define meta5-length
;;     ((lambda (mk-length)
;;        (mk-length mk-length))
;;      (lambda (mk-length)
;;        ((lambda (length)
;;           (lambda (l)
;;             (cond ((null? l) 0)
;;                   (else (add1 (length (cdr l)))))))
;;         (mk-length mk-length)))))
;;
;; but this doesn't quite work, because it no longer returns a
;; function to recurse on. So wrap that up in an extra lambda

(define meta5-length
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     ((lambda (length)
        (lambda (l)
          (cond ((null? l) 0)
                (else (add1 (length (cdr l)))))))
      (lambda (x) ((mk-length mk-length) x))))))

(test-length "meta5-length" meta5-length meta5-length meta5-length)

;; factor length out to the outside since it no longer depends on mk-length
(define meta6-length
  ((lambda (le)
     ((lambda (mk-length)
        (mk-length mk-length))
      (lambda (mk-length)
        (le
         (lambda (x) ((mk-length mk-length) x))))))
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))

(test-length "meta6-length" meta6-length meta6-length meta6-length)

;; extract out Y combinator

(define Y
  (lambda (le)
     ((lambda (f) (f f))
      (lambda (f)
        (le (lambda (x) ((f f) x)))))))

(define meta7-length
  (Y
   (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else (add1 (length (cdr l)))))))))

(test-length "meta7-length" meta7-length meta7-length meta7-length)

