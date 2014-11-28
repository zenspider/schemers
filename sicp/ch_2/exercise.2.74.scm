#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;;; Exercise 2.74

;; Insatiable Enterprises, Inc., is a highly
;; decentralized conglomerate company consisting of a large number of
;; independent divisions located all over the world.  The company's
;; computer facilities have just been interconnected by means of a
;; clever network-interfacing scheme that makes the entire network
;; appear to any user to be a single computer.  Insatiable's
;; president, in her first attempt to exploit the ability of the
;; network to extract administrative information from division files,
;; is dismayed to discover that, although all the division files have
;; been implemented as data structures in Scheme, the particular data
;; structure used varies from division to division.  A meeting of
;; division managers is hastily called to search for a strategy to
;; integrate the files that will satisfy headquarters' needs while
;; preserving the existing autonomy of the divisions.
;;
;; Show how such a strategy can be implemented with data-directed
;; programming.  As an example, suppose that each division's
;; personnel records consist of a single file, which contains a set
;; of records keyed on employees' names.  The structure of the set
;; varies from division to division.  Furthermore, each employee's
;; record is itself a set (structured differently from division to
;; division) that contains information keyed under identifiers such
;; as `address' and `salary'.  In particular:
;;
;;   a. Implement for headquarters a `get-record' procedure that
;;      retrieves a specified employee's record from a specified
;;      personnel file.  The procedure should be applicable to any
;;      division's file.  Explain how the individual divisions' files
;;      should be structured.  In particular, what type information
;;      must be supplied?

(define db
  '(((x fred) (ssn 123) (salary 24))
    ((y sue) (ssn 456) (salary 42))))

(define (get-record dept name)
  (let ((id (list dept name)))
    (define (iterate db)
      (if (null? db) null
          (if (equal? id (caar db))
              (cdar db)
              (iterate (cdr db)))))
    (iterate db)))

(assert-equal '((ssn 123) (salary 24)) (get-record 'x 'fred))
(assert-equal '((ssn 456) (salary 42)) (get-record 'y 'sue))
(assert-equal null  (get-record 'z 1))

;;   b. Implement for headquarters a `get-salary' procedure that
;;      returns the salary information from a given employee's record
;;      from any division's personnel file.  How should the record be
;;      structured in order to make this operation work?

;; yes, I used assoc. sue me. It takes 4 lines to write one of your own.

(define (get-salary dept name)
  (let ((record (get-record dept name)))
    (and record (assoc 'salary record))))

(assert-equal '(salary 24) (get-salary 'x 'fred))

;;   c. Implement for headquarters a `find-employee-record'
;;      procedure.  This should search all the divisions' files for
;;      the record of a given employee and return the record.  Assume
;;      that this procedure takes as arguments an employee's name and
;;      a list of all the divisions' files.

;; huh? what's the difference between this and get-record? The unique
;; id for a record is the dept and name, so pretending that the name
;; will be unique now is a really bad idea!

(define (find-employee-record name db)
  (define (iterate db)
    (if (null? db) null
        (if (equal? name (cadaar db))
            (car db)
            (iterate (cdr db)))))
  (iterate db))

(assert-equal (car db) (find-employee-record 'fred db))

;;   d. When Insatiable takes over a new company, what changes must
;;      be made in order to incorporate the new personnel information
;;      into the central system?

;; A: the "dept" tag needs to be unique, that's it... ignoring
;; find-employee-record's horrible concept

(done)
