#!/usr/bin/env csi -s

(use test)
(require-library logic-eval)
(import logic-eval)
(initialize-data-base microshaft-data-base)

;;; Exercise 4.59

;; Ben Bitdiddle has missed one meeting too many.
;; Fearing that his habit of forgetting meetings could cost him his
;; job, Ben decides to do something about it.  He adds all the weekly
;; meetings of the firm to the Microshaft data base by asserting the
;; following:

(assert! '(meeting accounting     (Monday    9am)))
(assert! '(meeting administration (Monday    10am)))
(assert! '(meeting computer       (Wednesday 3pm)))
(assert! '(meeting administration (Friday    1pm)))

;; Each of the above assertions is for a meeting of an entire
;; division.  Ben also adds an entry for the company-wide meeting
;; that spans all the divisions.  All of the company's employees
;; attend this meeting.

(assert! '(meeting whole-company (Wednesday 4pm)))

;;   a. On Friday morning, Ben wants to query the data base for all
;;      the meetings that occur that day.  What query should he use?

(test '((meeting administration (Friday 1pm)))
      (all-of '(meeting ?dept (Friday ?time))))

;;   b. Alyssa P. Hacker is unimpressed.  She thinks it would be much
;;      more useful to be able to ask for her meetings by specifying
;;      her name.  So she designs a rule that says that a person's
;;      meetings include all `whole-company' meetings plus all
;;      meetings of that person's division.  Fill in the body of
;;      Alyssa's rule.
;;
;;           (rule (meeting-time ?person ?day-and-time)
;;                 <RULE-BODY>)

(assert! '(rule (meeting-time ?who ?when)
                (or (meeting whole-company ?when)
                    (and (job ?who (?dept . ?iggy1))
                         (meeting ?dept ?when)))))

(test '((meeting-time (Hacker Alyssa P) (Wednesday 4pm))
        (meeting-time (Hacker Alyssa P) (Wednesday 3pm)))
      (all-of '(meeting-time (Hacker Alyssa P) ?when)))

;;   c. Alyssa arrives at work on Wednesday morning and wonders what
;;      meetings she has to attend that day.  Having defined the
;;      above rule, what query should she make to find this out?

;; this is really fucking ugly...

(test '((and (meeting-time (Hacker Alyssa P) (Wednesday 4pm))
             (meeting whole-company (Wednesday 4pm)))
        (and (meeting-time (Hacker Alyssa P) (Wednesday 3pm))
             (meeting computer (Wednesday 3pm))))
      (all-of '(and (meeting-time (Hacker Alyssa P) (Wednesday ?when))
                    (meeting ?div (Wednesday ?when)))))

;; this should make it prettier:

(assert! '(rule (my-meetings ?who ?day ?time ?div)
                (and (meeting-time ?who (?day ?time))
                     (meeting ?div (?day ?time)))))

(test '((my-meetings (Hacker Alyssa P) Wednesday 4pm whole-company)
        (my-meetings (Hacker Alyssa P) Wednesday 3pm computer))
      (all-of '(my-meetings (Hacker Alyssa P) Wednesday ?time ?what)))
