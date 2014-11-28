#!/usr/bin/env csi -s

(require rackunit)
(use amb amb-extras)

;;; Exercise 4.43

;; Use the `amb' evaluator to solve the following puzzle:(2)
;;
;;      Mary Ann Moore's father has a yacht and so has each of his
;;      four friends: Colonel Downing, Mr. Hall, Sir Barnacle Hood,
;;      and Dr. Parker. Each of the five also has one daughter and
;;      each has named his yacht after a daughter of one of the
;;      others. Sir Barnacle's yacht is the Gabrielle, Mr. Moore owns
;;      the Lorna; Mr. Hall the Rosalind. The Melissa, owned by
;;      Colonel Downing, is named after Sir Barnacle's daughter.
;;      Gabrielle's father owns the yacht that is named after Dr.
;;      Parker's daughter. Who is Lorna's father?
;;
;; Try to write the program so that it runs efficiently (see *Note
;; Exercise 4-40::). Also determine how many solutions there are if we
;; are not told that Mary Ann's last name is Moore.

(define dude  car)
(define yacht cadr)
(define bitch caddr)
(define (which-bitch name dudes) (find (lambda (d) (eq? (bitch d) name)) dudes))

(define (rich-bitches)
  (let* ((mor (list 'mor 'l                  'a ))
         (col (list 'col 'm (amb 'l    'r 'g   )))
         (hal (list 'hal 'r (amb 'l       'g   )))
         (bar (list 'bar 'g         'm          ))
         (drp (list 'drp 'a (amb 'l    'r      )))
         (dudes (list mor col hal bar drp)))

    (amb-assert (distinct? (map bitch dudes)))
    (amb-assert (eq? (bitch drp) (yacht (which-bitch 'g dudes))))

    dudes))

(all-of (dude (which-bitch 'l (rich-bitches)))) ; 15 checks



