;; #!/usr/bin/env csi -s

(require rackunit)

(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.55

;; Give simple queries that retrieve the following
;; information from the data base:
;;
;;   1. all people supervised by Ben Bitdiddle;

(test '((supervisor (Tweakit Lem E) (Bitdiddle Ben))
        (supervisor (Fect Cy D) (Bitdiddle Ben))
        (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
      (all-of '(supervisor ?x (Bitdiddle Ben))))

;;   2. the names and jobs of all people in the accounting division;

(test '((job (Cratchet Robert) (accounting scrivener))
        (job (Scrooge Eben) (accounting chief accountant)))
      (all-of '(job ?x (accounting . ?type))))

;;   3. the names and addresses of all people who live in Slumerville.

(test '((address (Aull DeWitt) (Slumerville (Onion Square) 5))
        (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
        (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
 (all-of '(address ?x (Slumerville . ?stuff))))

