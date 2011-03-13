#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.67

;; Devise a way to install a loop detector in the
;; query system so as to avoid the kinds of simple loops illustrated
;; in the text and in *Note Exercise 4-64::.  The general idea is that
;; the system should maintain some sort of history of its current
;; chain of deductions and should not begin processing a query that
;; it is already working on.  Describe what kind of information
;; (patterns and frames) is included in this history, and how the
;; check should be made.  (After you study the details of the
;; query-system implementation in section *Note 4-4-4::, you may want
;; to modify the system to include your loop detector.)
