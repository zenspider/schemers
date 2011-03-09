#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.6

;; `Let' expressions are derived expressions, because
;;
;;      (let ((<VAR_1> <EXP_1>) ... (<VAR_N> <EXP_N>))
;;        <BODY>)
;;
;; is equivalent to
;;
;;      ((lambda (<VAR_1> ... <VAR_N>)
;;         <BODY>)
;;       <EXP_1>
;;       ...
;;       <EXP_N>)
;;
;; Implement a syntactic transformation `let->combination' that
;; reduces evaluating `let' expressions to evaluating combinations of
;; the type shown above, and add the appropriate clause to `eval' to
;; handle `let' expressions.
